*          DATA SET REREP3502  AT LEVEL 046 AS OF 05/01/02                      
*PHASE RE3502C,*                                                                
         TITLE 'REREP3502 - RE3502 - GROUP BUY TAPE'                            
**********************************************************************          
* UNDER MVS, AN UNLABELLED TAPE IS CREATED BY MEANS OF LABEL=(,NL)   *          
* PARAMETER ON THE APPROPRIATE DD STATEMENT FOR THE OUTPUT TAPE.     *          
**********************************************************************          
         SPACE 2                                                                
********************************************************************            
*                                                                  *            
*        REREP3502 --- PRODUCE GROUP BUY TAPE                      *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
*                                                                  *            
* FEB20/89 (MRR) --- REMOVE HARD CODED REP (COMPANY) CODES AND     *            
*                     'QREP' TO FILTER DATA FOR THE OUTPUT TAPE    *            
*                                                                  *            
* OCT19/89 (MRR) --- NEED TO FORCE QREP INTO KEY ON FILE HIGHS AND *            
*                     SEQ(S)                                       *            
*                                                                  *            
*                                                                  *            
* 10/20/89  PJS  --- FIX ADV/AGENCY READ FOR INTEREP MASTER FILE   *            
*                    (CAN'T SKIP READ---MUST 'SEQ' THRU KEYS)      *            
*                                                                  *            
*                    FIX DMGR DIRECTORY INTERFACE FOR KEYSAVE,KEY  *            
*                    CONTRUCT. (CHANGE FROM KEY,KEY)               *            
*                                                                  *            
* FEB07/91 (MRR) --- CHANGE DIVISION CODES                         *            
*                                                                  *            
* DEC04/91 (BU ) --- INSTALL VALUENEW FACILITY                     *            
*                                                                  *            
* MAR27/92 (BU ) --- MAKE COMPATIBLE WITH VALU2NEW                 *            
*                    REREPRGEQU ---> REREPRGEQA                    *            
*                                                                  *            
* DEC11/95 (BG ) --- 2K CONTRACTS REGENALL REGENALL1               *            
*                                                                   *           
* JAN25/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
********************************************************************            
*                                                                               
RE3502   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE3502                                                       
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         EJECT                                                                  
         SPACE 1                                                                
GB5      CLI   MODE,REQFRST                                                     
         BNE   GBUY0012                                                         
         SPACE 1                                                                
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         SPACE 1                                                                
* CREATE TAPE WITH AGENCY, ADVERTISER, SALESPERSON AND RATE CARD FILES          
* COMBINING ALL BLAIR RELATED COMPANIES (SEE REPLIST)                           
         SPACE 1                                                                
         CLI   QOPTION2,C'C'       OPTION TO CREATE FILE TAPE                   
         BNE   GBUY0010                                                         
         SPACE 1                                                                
         MVI   RCSUBPRG,1          HEADLINES FOR FILE TAPE                      
         LA    R8,FILE                                                          
         OPEN  (FILE,OUTPUT)       OPEN FILE TAPE                               
         SPACE 1                                                                
         ZAP   COUNTADV,=P'0'                                                   
         ZAP   COUNTAGY,=P'0'                                                   
         ZAP   COUNTMAN,=P'0'                                                   
         ZAP   COUNTRAT,=P'0'                                                   
         SPACE 1                                                                
         XC    KEY,KEY             GET ADVERTISERS IN NUMERIC SEQUENCE          
         MVI   KEY,X'08'                                                        
         MVC   KEY+25(2),QREP                                                   
         BAS   RE,HIGH                                                          
ADV10    CLI   KEY,X'08'                                                        
         BNE   AGENCY                                                           
         CLC   KEY+25(2),QREP                                                   
         BNE   ADV40                                                            
ADV30    BAS   RE,GETADV                                                        
         TM    RADVCNTL,X'80'                                                   
         BO    ADV40                                                            
         XC    WRK,WRK                                                          
         LA    R6,WRK                                                           
         USING ADVD,R6                                                          
         MVI   ADVTYP,X'08'                                                     
         MVC   ADVREP,QREP                                                      
         MVC   ADVCD,RADVKADV                                                   
         MVC   ADVNM,RADVNAME                                                   
         PUT   (R8),(R6)                                                        
         AP    COUNTADV,=P'1'                                                   
         SPACE 1                                                                
*                                                                               
*- SKIP-READ DOES NOT WORK FOR INTEREP SUBSIDIARY KEYS                          
*                                                                               
ADV40    EQU   *                                                                
****     MVC   KEY+25(2),=C'ZZ'                                                 
****     BAS   RE,HIGH                                                          
****     CLI   KEY,X'08'                                                        
****     BNE   AGENCY                                                           
         MVC   KEY+25(2),QREP                                                   
         BAS   RE,SEQ                                                           
         B     ADV10                                                            
         DROP  R6                                                               
         SPACE 3                                                                
AGENCY   XC    KEY,KEY             GET AGENCIES                                 
         MVI   KEY,X'0A'                                                        
         MVC   KEY+25(2),QREP                                                   
         BAS   RE,HIGH                                                          
AGY10    CLI   KEY,X'0A'                                                        
         BNE   MAN                                                              
         CLC   KEY+25(2),QREP                                                   
         BNE   AGY40                                                            
AGY30    BAS   RE,GETAGY                                                        
         TM    RAGYCNTL,X'80'                                                   
         BO    AGY40                                                            
         XC    WRK,WRK                                                          
         LA    R6,WRK                                                           
         USING AGYD,R6                                                          
         MVI   AGYTYP,X'0A'                                                     
         MVC   AGYREP,QREP                                                      
         MVC   AGYCD,RAGYKAGY                                                   
         MVC   AGYOF,RAGYKAOF                                                   
         MVC   AGYNM,RAGYNAM1                                                   
         PUT   (R8),(R6)                                                        
         AP    COUNTAGY,=P'1'                                                   
         SPACE 1                                                                
*                                                                               
*- SKIP-READ DOES NOT WORK FOR INTEREP SUBSIDIARY KEYS                          
*                                                                               
AGY40    EQU   *                                                                
*****    MVC   KEY+25(2),=C'ZZ'                                                 
*****    BAS   RE,HIGH                                                          
*****    CLI   KEY,X'0A'                                                        
*****    BNE   MAN                                                              
         MVC   KEY+25(2),QREP                                                   
         BAS   RE,SEQ                                                           
         B     AGY10                                                            
         DROP  R6                                                               
         SPACE 3                                                                
MAN      EQU   *                                                                
MAN10    XC    KEY,KEY                                                          
         MVI   KEY,X'06'           GET SALESPERSON                              
         MVC   KEY+22(2),QREP                                                   
         BAS   RE,HIGH                                                          
MAN20    CLI   KEY,X'06'                                                        
         BNE   MAN25                                                            
         CLC   KEY(24),KEYSAVE     SAME REP                                     
         BE    MAN30                                                            
MAN25    EQU   *                                                                
         B     RATE                                                             
         SPACE 1                                                                
MAN30    BAS   RE,GETMAN                                                        
         TM    RSALCNTL,X'80'                                                   
         BO    MAN40                                                            
         XC    WRK,WRK                                                          
         LA    R6,WRK                                                           
         USING MAND,R6                                                          
         MVI   MANTYP,X'06'                                                     
         MVC   MANREP,RSALKREP                                                  
         MVC   MANCD,RSALKSAL                                                   
         MVC   MANNM,RSALNAME                                                   
         MVC   MANTM,RSALTEAM                                                   
         PUT   (R8),(R6)                                                        
         AP    COUNTMAN,=P'1'                                                   
MAN40    BAS   RE,SEQ                                                           
         B     MAN20                                                            
         DROP  R6                                                               
         SPACE 3                                                                
RATE     DS    0H'0'                                                            
         OPEN  (RATES,INPUT)                                                    
READ     GET   RATES,CARD                                                       
         SPACE 1                                                                
         XC    WRK,WRK                                                          
         LA    R6,WRK                                                           
         USING RATD,R6                                                          
         MVI   RATTYP,X'0D'                                                     
         MVC   RATREP,CARD+60                                                   
         MVC   RATSTA,CARD+9                                                    
         MVC   RATRAT1,CARD+15                                                  
         MVC   RATRAT2,CARD+22                                                  
         MVC   RATRAT3,CARD+29                                                  
         PUT   (R8),(R6)                                                        
         AP    COUNTRAT,=P'1'                                                   
         B     READ                                                             
         DROP  R6                                                               
         SPACE 1                                                                
RATEX    CLOSE (FILE,)             CLOSE THE FILE TAPE                          
         CLOSE (RATES,)            CLOSE THE RATE FILE                          
         SPACE 1                                                                
         LA    R2,P+22                                                          
         LA    R4,COUNTADV                                                      
         MVC   P+5(11),=C'ADVERTISERS'                                          
         BAS   RE,EDIT1                                                         
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         SPACE 1                                                                
         LA    R4,COUNTAGY                                                      
         MVC   P+5(8),=C'AGENCIES'                                              
         BAS   RE,EDIT1                                                         
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         SPACE 1                                                                
         LA    R4,COUNTMAN                                                      
         MVC   P+5(11),=C'SALESPEOPLE'                                          
         BAS   RE,EDIT1                                                         
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         SPACE 1                                                                
         LA    R4,COUNTRAT                                                      
         MVC   P+5(5),=C'RATES'                                                 
         BAS   RE,EDIT1                                                         
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         SPACE 1                                                                
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         B     GBUY0010                                                         
         SPACE 1                                                                
EDIT1    EDIT  (P4,0(R4)),(7,0(R2)),COMMAS=YES                                  
         BR    RE                                                               
         SPACE 1                                                                
* NOW, OPEN THE BUY DATA TAPE                                                   
GBUY0010 LA    R8,BLGB                                                          
         OPEN  (BLGB,OUTPUT)                                                    
         SPACE 1                                                                
         MVI   RCSUBPRG,0          HEADLINES FOR BUY DATA TAPE                  
         ZAP   COUNTBUY,=P'0'                                                   
         ZAP   GROSS,=P'0'                                                      
         B     GBUY0099                                                         
         EJECT                                                                  
GBUY0012 CLI   MODE,PROCCONT                                                    
         BNE   GBUY0056                                                         
         LA    R8,BLGB                                                          
         XC    WRK,WRK                                                          
         LA    R6,WRK                                                           
         USING BUYD,R6                                                          
         MVC   TREP(76),SPACES                                                  
         MVI   TDIV,C'1'                                                        
         CLC   RCONKGRP,=C'TG'        WAS 'RA'                                  
         BE    GBUY0016                                                         
         MVI   TDIV,C'2'                                                        
         CLC   RCONKGRP,=C'TR'        WAS 'RF'                                  
         BE    GBUY0016                                                         
         MVI   TDIV,C'3'                                                        
         CLC   RCONKGRP,=C'TB'        WAS 'RS'                                  
         BE    GBUY0016                                                         
         MVI   TDIV,C'4'                                                        
         CLC   RCONKGRP,=C'TA'                                                  
         BE    GBUY0016                                                         
         MVI   TDIV,C'5'                                                        
         CLC   RCONKGRP,=C'TC'                                                  
         BE    GBUY0016                                                         
         MVI   TDIV,C'6'                                                        
         CLC   RCONKGRP,=C'TN'                                                  
         BE    GBUY0016                                                         
         MVI   TDIV,C'7'                                                        
         CLC   RCONKGRP,=C'RX'                                                  
         BE    GBUY0016                                                         
         MVI   TDIV,C'8'                                                        
         CLC   RCONKGRP,=C'RC'                                                  
         BE    GBUY0016                                                         
         MVI   TDIV,C'0'                                                        
         CLC   RCONKGRP,=C'RW'                                                  
         BE    GBUY0016                                                         
         MVI   TDIV,C'A'                                                        
         CLI   RCONKGRP,C'N'    ALL N                                           
         BE    GBUY0016                                                         
         MVI   TDIV,C'B'                                                        
         CLI   RCONKGRP,C'O'    ALL O                                           
         BE    GBUY0016                                                         
         MVI   TDIV,C'C'                                                        
         CLI   RCONKGRP,C'M'    ALL M                                           
         BE    GBUY0016                                                         
         MVI   TDIV,C'D'                                                        
         CLC   RCONKGRP,=C'T '     ALL OTHER T (BLAIRSPAN)                      
         BE    GBUY0016                                                         
         MVI   TDIV,C'E'                                                        
         CLC   RCONKGRP,=C'TI'                                                  
         BE    GBUY0016                                                         
         MVI   TDIV,C'9'           ANYTHING ELSE                                
         SPACE 1                                                                
GBUY0016 EQU   *                                                                
         MVC   TREP,QREP                                                        
         MVC   TSTAT,RCONKSTA                                                   
         MVC   TOFFC,RCONKOFF                                                   
         SPACE 1                                                                
GBUY0020 GOTO1 DATCON,DMCB,(3,RCONDATE+3),(0,WORK)                              
         MVC   TCNED(4),WORK+2                                                  
         MVC   TCNED+4(2),WORK                                                  
         MVC   TTYPE,RCONTYPE                                                   
         MVC   TCATC(2),RCONCTGY                                                
         MVC   TADVC,RCONKADV                                                   
         SPACE 1                                                                
         MVC   TPRDC,RCONPRD                                                    
         MVC   TPRD,RPRDNAME                                                    
         MVC   TAGYC,RCONKAGY                                                   
         MVC   TSALC,RCONSAL                                                    
         SPACE 1                                                                
         MVC   TAGYOF,RCONKAOF                                                  
         MVC   DUB(4),RCONKCON                                                  
         MVI   DUB+4,X'0F'                                                      
         UNPK  WORK(9),DUB(5)                                                   
         MVC   TCON,WORK                                                        
         EJECT                                                                  
       ++INCLUDE REREPRGEQA                                                     
*  FOR MONTH OF NOV86 -                                                         
*      REQUEST 10/86-12/86 FOR ESTIMATE REPORT - QOPTION4=A                     
*                             (BUT ONLY WANT DATA FROM MIDDLE MONTH)            
*                                                                               
*            FOR 12/86-IGNORE                                                   
*            FOR 11/86-GIVE TOTAL AMOUNT OF ORDERED                             
*            FOR 10/86-IGNORE                                                   
*                                                                               
*      REQUEST 12/85-11/86 FOR ACTUAL REPORT                                    
*           FOR 11/86- IGNORE (ESTIMATE MONTH)                                  
*           FOR 10/86-GIVE TOTAL AMOUNT OF INVOICES, IF THERE ARE               
*                      ANY (EVEN IF $0), OR GIVE TOTAL AMOUNT OF                
*                      ORDERED                                                  
*           FOR 9/86 AND PRIOR - GIVE ADJUSTMENTS ONLY                          
*                     IF NO INVOICES AT ALL,                                    
*                     GIVE ESTIMATES THIS MONTH                                 
*                     IF INVOICES NOW, BUT NO PRIOR INVOICES,                   
*                     GIVE INVOICES LESS ALL PREVIOUS ORDERED                   
*                                            --------                           
*                     IF INVOICES NOW AND PRIOR INVOICES ALSO,                  
*                     GIVE INVOICES THIS MONTH                                  
         SPACE 2                                                                
GBUY0024 L     R2,ANEWMON          A(NEW MONTH TABLE)                           
GBUY0025 EQU   *                                                                
         CLC   0(4,R2),QSTART      TABLE ENTRY VS REQ START DATE                
         BE    GBUY0028            FOUND - DO WHAT YOU DO SO WELL               
         LA    R2,NEXTBUCK(R2)     BUMP TO NEXT BUCKET                          
         B     GBUY0025            GO BACK FOR NEXT                             
GBUY0028 EQU   *                                                                
         CLC   0(4,R2),QEND        TABLE ENTRY VS REQ END   DATE                
         BH    GBUY0099            TABLE > END DATE - EXIT                      
*                                                                               
*   MONTABLE FORMAT HAS CHANGED.  LAST MONTH OF REQUEST IS NO LONGER            
*     FOLLOWED BY A MONTH OF ZERO.  THEREFORE, THE MONTH IN PROCESS             
*     IS TO BE FOUND IN THE MONINFO TABLE, AND THAT ADDRESS USED                
*     TO CHECK WHETHER LAST MONTH/PENULTIMATE MONTH HAS BEEN REACHED.           
*     MONINFO TABLE CONTAINS ONLY THOSE MONTHS IN REQUEST.                      
*                                                                               
         BAS   RE,CHKKEYMO         LOCATE MONTH IN MONINFO TABLE                
         L     R7,FULL             SET RETURNED ADDRESS                         
         LR    R4,R2               SET A(BUCKETS IN MONTH)                      
         LA    R4,BUCKDISP(R4)     BYPASS MONTH CONTROL                         
         CLI   16(R7),0            IGNORE LAST REQUESTED MONTH FOR              
         BE    GBUY0099            BOTH ESTIMATE AND ACTUAL TAPE                
         SPACE 1                                                                
         CLI   QOPTION4,C'A'       A=ESTIMATE REPORT                            
         BNE   GBUY0032                                                         
         CLI   32(R7),0            FOR ESTIMATE TAPE, ONLY WANT                 
         BNE   GBUY0052            MIDDLE MONTH                                 
         SPACE 1                                                                
         L     R5,TOTORD(R4)       USE ORDERED THIS MONTH                       
         A     R5,CUASATIN(R4)     + PREVIOUS ORDERED                           
         B     GBUY0040                                                         
         SPACE 1                                                                
*                            FOR ACTUAL REPORT                                  
GBUY0032 CLI   32(R7),0                                                         
         BE    GBUY0036                                                         
         L     R5,PRASATOR(R4)     SHOW ACCT. ADJMNTS                           
         B     GBUY0040                                                         
         SPACE 1                                                                
GBUY0036 L     R5,GROSSORD(R4)     USE INVOICE AMOUNT IF PRESENT                
         TM    FLAG6(R2),X'01'                                                  
         BO    GBUY0044            EVEN IF $0                                   
         SPACE 1                                                                
         L     R5,TOTORD(R4)       ELSE USE ORDERED THIS MONTH                  
         A     R5,CUASATIN(R4)     PLUS PREVIOUS ORDERED                        
         L     R3,GROSSORD(R4)                                                  
         LTR   R3,R3         ALWAYS PRINT IF PREV ORDERED WAS NON-0             
         BNZ   GBUY0044            EVEN IF TOTAL IS ZERO                        
*                                 (FOR +/- CASE)                                
         SPACE 2                                                                
GBUY0040 LTR   R5,R5                                                            
         BZ    GBUY0052                                                         
         SPACE 1                                                                
GBUY0044 CVD   R5,DUB                                                           
         UNPK  TGROSS,DUB+3(5)                                                  
         TM    TGROSS+8,X'10'                                                   
         BO    *+8                                                              
         OI    TGROSS+8,X'F0'                                                   
         SPACE 1                                                                
         MVC   TCLDT(2),2(R2)      MM  FROM MONTH TABLE ENTRY                   
         MVC   TCLDT+2(2),0(R2)    YY  FROM MONTH TABLE ENTRY                   
         SPACE 1                                                                
GBUY0048 PUT   (R8),(R6)                                                        
         AP    COUNTBUY,=P'1'                                                   
         AP    GROSS,DUB+3(5)                                                   
         SPACE 1                                                                
GBUY0052 LA    R2,NEXTBUCK(R2)                                                  
         B     GBUY0028                                                         
         DROP  R6                                                               
         EJECT                                                                  
GBUY0056 CLI   MODE,REQLAST                                                     
         BNE   GBUY0099                                                         
         SPACE 1                                                                
         CLOSE (BLGB,)             CLOSE BUY DATA FILE                          
         SPACE 1                                                                
         LA    R2,P+22                                                          
         LA    R3,P+35                                                          
         LA    R4,COUNTBUY                                                      
         LA    R5,GROSS                                                         
         MVC   P+5(5),=C'TOTAL'                                                 
         SPACE 1                                                                
         EDIT  (P4,0(R4)),(7,0(R2)),COMMAS=YES                                  
         EDIT  (P6,0(R5)),(15,0(R3)),2,MINUS=YES,COMMAS=YES                     
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         SPACE 1                                                                
GBUY0099 XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CHKKEYMO --- ROUTINE USES POSITION WITHIN THE OUTPUT TABLE,                 
*      ADDRESS IN 'ANEWMON', TO DETERMINE THE DATE BEING PROCESSED.             
*      THE TWO-BYTE YYMM OF THE 'CURRENT DATE' FOR THAT MONTH IS                
*      DISPLACED 'CURDATE' BYTES IN.  THIS DATE IS THEN FOUND IN THE            
*      'MONINFO' TABLE, WHICH CORRESPONDS TO THE DATES OF THE REQUEST.          
*      ONCE FOUND, ITS ADDRESS IS PASSED BACK TO THE CALLER IN 'FULL'.          
*                                                                               
CHKKEYMO NTR1                                                                   
         L     R3,AMONINFO         A(MONTH INFORMATION TABLE)                   
CKEY0002 EQU   *                                                                
         CLC   0(2,R3),CURDATE(R2) MONINFO VS O/P TABLE CURR DATE               
         BE    CKEY0006            FOUND                                        
         LA    R3,16(R3)           BUMP TO NEXT MONINFO BUCKET                  
         CLI   0(R3),0             ANY DATE IN BUCKET?                          
         BNE   CKEY0002            YES - CHECK NEXT                             
         DC    H'0'                SHOULDN'T HAPPEN                             
CKEY0006 EQU   *                                                                
         ST    R3,FULL             PASS BACK ADDRESS OF BUCKET                  
         XIT1                                                                   
         EJECT                                                                  
*        DATA MANAGER INTERFACE (DIRECTORY)                                     
*                                                                               
         SPACE                                                                  
HIGH     LA    RF,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         B     LINKDIR                                                          
*                                                                               
SEQ      LA    RF,DMRSEQ                                                        
         MVC   KEYSAVE,KEY                                                      
         B     LINKDIR                                                          
*                                                                               
*- CHANGE CALL TO KEYSAVE,KEY FORMAT FROM KEY,KEY FORMAT.                       
*  (FOR INTEREP MASTER FILE RULE WITH NEW DMGR)                                 
LINKDIR  NTR1                                                                   
         ST    RF,DMCB                                                          
         MVC   TRACEKEY,KEY                                                     
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,,REPDIR,KEYSAVE,KEY,0                               
         B     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
*        DATA MANAGER INTERFACE (FILE GETS)                                     
*                                                                               
         SPACE                                                                  
GETAGY   LA    RF,RAGYREC                                                       
         B     LINKFILE                                                         
*                                                                               
GETADV   LA    RF,RADVREC                                                       
         B     LINKFILE                                                         
*                                                                               
GETMAN   LA    RF,RSALREC                                                       
         B     LINKFILE                                                         
*                                                                               
*                                                                               
LINKFILE NTR1                                                                   
         LR    R2,RF                                                            
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),REPFILE,KEY+28,           X        
               (R2),(0,DMWORK)                                                  
         MVC   LASTIO,DMCB+12      SAVE THESE VALUES                            
         MVC   LASTDA,KEY+28                                                    
         MVC   LASTLEN,27(R2)                                                   
         B     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
*        DATA MANAGER INTERFACE (CHECK ERRORS)                                  
*                                                                               
         SPACE                                                                  
DMCHECK  TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BZ    DM010                                                            
         TM    29(R2),X'80'        IS RECORD MARKED FOR DELETION                
         BZ    DM020               NO - ERROR                                   
         LTR   RB,RB               YES - RETURN WITH CC NE 0                    
         B     GBUY0099                                                         
*                                                                               
DM010    TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    GBUY0099                                                         
*                                                                               
DM020    MVC   WORK(41),=C'**********DATA MANAGER ERROR*************'           
         GOTO1 LOGIO,WORK+48,1,(41,WORK)                                        
         MVC   WORK(41),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'            BLOW UP                                          
         EJECT                                                                  
*                                                                               
*        ROUTINE TO TRACE DATA MANAGER CALLS                                    
*                                                                               
         SPACE                                                                  
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   TRACEDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         LA    R4,TRACEKEY                                                      
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACE4                                                         
*                                                                               
DMTRACE2 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
*                                                                               
DMTRACE4 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,TRACEDM8,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     GBUY0099                                                         
         SPACE 2                                                                
TRACEDM8 DS    C                                                                
         EJECT                                                                  
*  TAPE FOR BUY DATA                                                            
         SPACE 1                                                                
BLGB     DCB   DDNAME=BLGB,                                            X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00076,                                            X        
               BLKSIZE=07600,                                          X        
               MACRF=PM                                                         
         SPACE 4                                                                
*        TAPE FOR FILE DATA                                                     
         SPACE 1                                                                
FILE     DCB   DDNAME=FILE,                                            X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00029,                                            X        
               BLKSIZE=02900,                                          X        
               MACRF=PM                                                         
         SPACE 4                                                                
*        FILE FOR RATE DATA                                                     
         SPACE 1                                                                
RATES    DCB   DDNAME=RATES,                                           X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               EODAD=RATEX,                                            X        
               MACRF=GM                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
COUNTBUY DS    PL4                 BUY DATA RECORDS                             
GROSS    DS    PL6                                                              
COUNTADV DS    PL4                                                              
COUNTAGY DS    PL4                                                              
COUNTMAN DS    PL4                                                              
COUNTRAT DS    PL4                                                              
CARD     DS    CL80                                                             
TRACEKEY DS    CL32                                                             
WRK      DS    CL75                                                             
         SPACE 3                                                                
*  REGENALL1A                                                                   
*  REREPWORKD                                                                   
*  REREPMODES                                                                   
*        PRINT OFF                                                              
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
         PRINT ON                                                               
         SPACE 2                                                                
* DSECT TO COVER ADVERTISER RECORD                                              
         SPACE 1                                                                
ADVD     DSECT                                                                  
ADVTYP   DS    XL1                 X'08'                                        
ADVREP   DS    CL2                 REP                                          
ADVCD    DS    CL4                 CODE                                         
ADVNM    DS    CL20                NAME                                         
         DS    CL2                 SPARE                                        
         SPACE 2                                                                
* DSECT TO COVER AGENCY RECORD                                                  
         SPACE 1                                                                
AGYD     DSECT                                                                  
AGYTYP   DS    XL1                 X'0A'                                        
AGYREP   DS    CL2                 REP                                          
AGYCD    DS    CL4                 CODE                                         
AGYOF    DS    CL2                 AGENCY OFFICE                                
AGYNM    DS    CL20                NAME                                         
         SPACE 2                                                                
* DSECT TO COVER SALESPERSON RECORD                                             
         SPACE 1                                                                
MAND     DSECT                                                                  
MANTYP   DS    XL1                 X'06'                                        
MANREP   DS    CL2                 REP                                          
MANCD    DS    CL3                 CODE                                         
MANTM    DS    CL2                 TEAM                                         
MANNM    DS    CL20                NAME                                         
         DS    CL1                 SPARE                                        
         SPACE 2                                                                
* DSECT TO COVER RATE CARD RECORD                                               
         SPACE 1                                                                
RATD     DSECT                                                                  
RATTYP   DS    XL1                 X'0D'                                        
RATREP   DS    CL2                 REP                                          
RATSTA   DS    CL5                 STATION                                      
RATRAT1  DS    CL6                 RATE 1                                       
RATRAT2  DS    CL6                 RATE 2                                       
RATRAT3  DS    CL6                 RATE 3                                       
         DS    CL3                 SPARE                                        
         SPACE 2                                                                
* DSECT TO COVER BUY DATA RECORDS                                               
         SPACE 1                                                                
BUYD     DSECT                                                                  
TREP     DS    CL2                 REP                                          
TDIV     DS    CL1                 SUB-GROUP                                    
TSTAT    DS    CL5                 STATION                                      
TOFFC    DS    CL2                 OFFICE                                       
TCLDT    DS    CL4                 CLOSE DATE MMYY                              
TGROSS   DS    CL9                 GROSS                                        
TCNED    DS    CL6                 CONTRACT END MMDDYY                          
TTYPE    DS    CL1                 TYPE                                         
TCATC    DS    CL2                 CATEGORY CODE                                
TADVC    DS    CL4                 ADVERTISER CODE                              
TPRDC    DS    CL3                 PRODUCT CODE                                 
TPRD     DS    CL20                PRODUCT NAME                                 
TAGYC    DS    CL4                 AGENCY CODE                                  
TAGYOF   DS    CL2                 AGENCY OFFICE                                
TCON     DS    CL8                 CONTRACT NUMBER                              
TSALC    DS    CL3                 SALESPERSON CODE                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046REREP3502 05/01/02'                                      
         END                                                                    
