*          DATA SET PPREPAE02  AT LEVEL 027 AS OF 03/08/16                      
*PHASE PPAE02A                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE CASHVAL                                                                
         TITLE 'PPAE02 - XML PROD BILLING'                                      
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
*  GHOA  03/16     CARAT XML BILLING                                            
*                                                                               
*  BPLA  01/15     NEW KRAFT CLEINT CODE - KRS (GROCERY?)                       
*                                                                               
*  BPLA  08/13     KRAFT - DYNAMIC ALLOCATION                                   
*                  USE KFTTYP - SET FROM FIRST RECORD                           
*                  IN THE INPUT FILE                                            
*                                                                               
*  BPLA  02/13     KRAFT - SPLIT FILES                                          
*                  PRODUCE PRTTAPE.PP0AEO02 (GRO) 3 (SNK)                       
*                  INSTEAD OF AN SFTPDISK FILE                                  
*                                                                               
*  BPLA  01/13     CHEVRON URL CHANGE                                           
*                                                                               
*  BPLA  04/12     CHANGES FOR A NEW FORMAT FOR KRAFT                           
*                                                                               
*  BPLA  05/11     UPPER CASE V ISSUE + NEW HUB TESTING FEATURE                 
*                  (v1) IN SCHEMEALOCATION                                      
*                                                                               
*  BPLA  01/11     CHG TO SUPPRESS LINE ITEM DETAILS FOR YN FILES               
*                                                                               
*  BPLA  9/30/09   NEW INPUT FILE FORMAT AND CHANGE & TO + IN JOB               
*                                                                               
*  BPLA  8/3/09    PUT CHEVRON WORKCODE ON A LineitemIdentier                   
*                  FIELD                                                        
*                                                                               
*  BPLA  7/20/09   MODIFICATION FROM PAUL HOLMES                                
*                  SCHEMALOCATION NEEDED WITH DATAOBJECTS                       
*                  AND ONLY FOR FIRST INVOICE                                   
*                                                                               
*  BPLA  7/9/09    MORE CHANGES - DATA OBJECTS AND INVOICE                      
*                                                                               
*  BPLA  6/11/09   CHANGE ORDER OF FIELDTICKETINFORMATION                       
*                  AND JOBLOCATIONINFORMATION                                   
*                                                                               
******* OPTIONS ********                                                        
*                                                                               
*        QPROG+52  (COL 53-56) =  4 CHARACTER AGENCY CODE                       
*                  ENTER 'TEST' FOR SPECIAL HUB TESTING                         
*                  ENTER 'TESTA' FOR ALSO ARIBIA                                
*                                                                               
*        QOPT2 Y=TEST RUN  - NO OUTPUT FILE                                     
*                                                                               
*        QOPT3 FOR KRAFT FILES (LBNY-O0)                                        
*              G- GROCERY FILE                                                  
*              S- SNACK FILE                                                    
*              NOW USE KFTTYP - SET FROM INPUT FILE                             
*                                                                               
*        QOPT4 T=TEST MQ NOTIFICATION                                           
*              N=NO MQ NOTIFICATION                                             
*                                                                               
*        QOPT5 X=USE NON-EXISTANT FILE NAME                                     
*              USE ONLY WITH QOPT2=Y?                                           
*                                                                               
         SPACE 1                                                                
PPAE02   CSECT                                                                  
         SPACE 1                                                                
         PRINT NOGEN                                                            
         NMOD1 0,PPAE02,R8,R9                                                   
         L     RC,0(R1)                                                         
         USING PPWORKD,RC         GLOBAL WORKING STORAGE                        
         L     RA,PPFILEC                                                       
         USING PPFILED,RA                                                       
         MVI   RC2DSECT,C'Y'      USE SECOND DSECT                              
         LA    R7,SPACEND                                                       
         USING LWSD,R7                                                          
*                                                                               
         L     R4,PPWORK2C                                                      
         USING PPWORK2D,R4                                                      
         MVC   ADMASTC,VMASTC                                                   
*                                                                               
         SPACE 2                                                                
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    FRST                                                             
         CLI   MODE,PROCREQ                                                     
         BE    RQFRST                                                           
         CLI   MODE,RUNLAST                                                     
         BE    LAST                                                             
*                                                                               
EQXIT    CR    RB,RB               SET CC EQUAL                                 
         B     XIT                                                              
*                                                                               
NEQXIT   LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         EJECT                                                                  
*=========================================================*                     
* RUNFRST PROCESSING                                      *                     
*=========================================================*                     
         SPACE 1                                                                
FRST     DS    0H                                                               
         RELOC RELO                                                             
*                                                                               
         MVI   TESTMQ,0                                                         
*                                                                               
         L     RE,=V(CASHVAL)                                                   
         A     RE,RELO                                                          
         ST    RE,VCASHVAL                                                      
*                                                                               
         L     RF,ADMASTC           USE MASTC'S MQRPT                           
         USING MASTD,RF                                                         
         MVC   AMQRPT,MCVMQRPT                                                  
         DROP  RF                                                               
*                                                                               
*                                                                               
         ZAP   RUNDOLS,=P'0'                                                    
         ZAP   RUNINVD,=P'0'       USED FOR KRAFT                               
         ZAP   RUNCTAX,=P'0'       USED FOR KRAFT                               
         ZAP   RUNINVS,=P'0'                                                    
*                                                                               
****     L     R8,PPWORK2C                                                      
****     USING PPWORK2D,R8                                                      
****                                                                            
****     USING MASTD,R1                                                         
****     ICM   R1,15,VMASTC                                                     
****     BNZ   *+6                                                              
****     DC    H'0'                                                             
****     MVC   VREMOTEC,MCVREMOT                                                
****     DROP  R1                                                               
****     DROP  R8                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(20,CTODAY) TODAY'S DATE YYYYMMDD              
*                                                                               
*        GET TIME OF DAY                                                        
         TIME                                                                   
*                                                                               
*        R0 NOW HAS TIME HHMMSSHS  (PWOS)                                       
*                                                                               
         ST    R0,FULL                                                          
         SRL   R0,4                                                             
         ST    R0,MYFULL                                                        
         XC    DUB,DUB                                                          
         MVC   DUB+5(3),MYFULL                                                  
         OI    DUB+7,X'0F'                                                      
         CVB   R6,DUB                                                           
         EDIT  (R6),(5,TIMEOFD),2,FILL=0                                        
         MVI   TIMEOFD+5,C'.'                                                   
         UNPK  WORK(3),FULL+2(2)                                                
         MVC   TIMEOFD+6(2),WORK     HH.MM.SS                                   
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
RQFRST   DS    0H                  REQUEST FIRST                                
*                                                                               
         MVI   KFTSW,C'N'          SET KRAFT SWITCH OFF                         
         MVI   CARSW,C'N'          SET CARAT SWITCH OFF                         
         MVI   YNASW,C'Y'          SET YNR ARIBIA SWITCH                        
         CLC   QPROG+52(4),=C'YNSF' CHECK FOR YN COMPANY ID'S                   
         BE    REQF1                                                            
         CLC   QPROG+52(4),=C'YRNY'                                             
         BE    REQF1                                                            
         CLC   QPROG+52(4),=C'YNWC'                                             
         BE    REQF1                                                            
         CLC   QPROG+52(4),=C'YNBR'                                             
         BE    REQF1                                                            
         CLC   QPROG+52(5),=C'TESTA' TESTING WITH ARIBIA                        
         BE    REQF1                                                            
         MVI   YNASW,C'N'          SET OFF YNR ARIBIA SWITCH                    
         MVI   KFTSW,C'Y'          SET OFF KRAFT FORMAT SWITCH                  
         CLC   QPROG+52(5),=C'KRAFT'   TEMPORARY FOR TESTING ONLY               
         BE    REQF1                                                            
         CLC   QPROG+52(4),=C'LBNY'    MVCTOK/MVCTOA                            
         BE    REQF1                                                            
         MVI   KFTSW,C'N'                                                       
         MVI   CARSW,C'Y'          SET CARAT FORMAT SWITCH                      
         CLC   QPROG+52(4),=C'UBNY'    CARAT                                    
         BE    REQF1                                                            
         MVI   CARSW,C'N'          SET OFF CARAT FORMAT SWITCH                  
*                                                                               
REQF1    MVC   RUNSUPL,SPACES      SUPPLIER ID                                  
         CLI   TESTMQ,0            ALREADY SET?                                 
         BNE   REQF1B                                                           
         MVC   TESTMQ,QOPT4                                                     
         CLI   TESTMQ,C' '          NOT ENTERED                                 
         BNE   *+8                                                              
         MVI   TESTMQ,C'Y'                                                      
*                                                                               
REQF1B   DS    0H                                                               
         MVC   SVQOPT2,QOPT2       SAVE FOR RUNLAST                             
         MVC   SVQOPT5,QOPT5       SAVE FOR RUNLAST                             
*                                                                               
         MVI   FIRSTINV,C'Y'                                                    
         XC    LASTINV,LASTINV                                                  
*                                                                               
         MVC   DSNAME,SPACES                                                    
         MVC   DSNAME+0(4),=C'BIL.'                                             
         MVC   DSNAME+4(3),=C'ACC'  ACC SYSTEM                                  
         MVI   DSNAME+7,C'.'                                                    
*                                                                               
         MVC   DSNAME+8(4),QPROG+52    4 CHARACTER AGY ID FROM REQ CARD         
*                                                                               
         MVC   DSNAME+12(2),=C'.D'                                              
         MVC   DSNAME+14(6),CTODAY+2    YYMMDD                                  
         MVC   DSNAME+20(2),=C'.T'                                              
         MVC   DSNAME+22(2),TIMEOFD         WITHOUT .'S                         
         MVC   DSNAME+24(2),TIMEOFD+3                                           
         MVC   DSNAME+26(2),TIMEOFD+6                                           
         MVC   MQMAPNM,=C'SFTPDISK.PROD.'                                       
         CLI   QOPT4,C'N'  NO NOTIFICATION - PUT TEST IN NAME                   
         BE    FRST05                                                           
         CLI   QOPT4,C'T'  PUTTING TO TEST BROKER?                              
         BNE   *+10                                                             
FRST05   MVC   MQMAPNM+9(4),=C'TEST'                                            
*                                                                               
         L     R1,=A(IO)                                                        
         ST    R1,AREC                                                          
         OPEN  (DATAFILE,(INPUT))  OPEN INPUT FILE                              
         OI    FCS,FCSIN                                                        
*                                                                               
FRST10   CLI   KFTSW,C'Y'          KRAFT FORMAT?                                
         BE    KFRST                                                            
         CLI   CARSW,C'Y'          CARAT FORMAT SIMILAR TO KRAFT                
         BE    KFRST                                                            
FRST10B  L     R1,=A(DATAFILE)                                                  
         LA    R2,REC                                                           
         USING CRECD,R2           FOR NOW MUST BE CHEVRON                       
FRST10N  GET   (1),(2)                                                          
         CLC   REC(50),SPACES       MAY ENCOUNTER EMPTY RECORDS                 
         BE    FRST10B              JUST IGNORE                                 
*                                                                               
*        BE SURE THAT NO BINARY ZEROS REMAIN IN THESE FIELDS                    
*                                                                               
         OC    CUSER1,SPACES                                                    
         OC    CUSER2,SPACES                                                    
         OC    CREF,SPACES                                                      
         OC    CWKTSK,SPACES                                                    
         OC    CCONTRA,SPACES                                                   
         OC    CACCBOTH,SPACES                                                  
*                                                                               
FRST15   CLC   CINVNUM,LASTINV      SAME INVOICE NUMBER?                        
         BE    FRST20                                                           
         BAS   RE,NEWINV                                                        
         MVC   LASTINV,CINVNUM                                                  
*                                                                               
FRST20   BAS   RE,PROCESS          PROCESS THE RECORD                           
         L     R1,=A(DATAFILE)                                                  
         LA    R2,REC                                                           
         GET   (1),(2)                                                          
         CLC   REC(50),SPACES       MAY ENCOUNTER EMPTY RECORDS                 
         BE    FRST10               JUST IGNORE                                 
         B     FRST15                                                           
*                                                                               
DATAEND  CLOSE (DATAFILE)          DONE                                         
         LTR   RF,RF                                                            
         BZ    XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
*        ROUTINE TO PROCESS KRAFT FORMAT                                        
*                                                                               
KFRST    L     R1,=A(DATAFILE)                                                  
         LA    R2,REC                                                           
         USING KRECD,R2                                                         
         MVI   FIRSTINV,C'Y'                                                    
         MVI   KFTTYP,C' '          CLEAR KRAFT TYPE                            
KFRST10N GET   (1),(2)                                                          
         CLC   REC(50),SPACES       MAY ENCOUNTER EMPTY RECORDS                 
         BE    KFRST10N             JUST IGNORE                                 
*                                                                               
         CLI   CARSW,C'Y'           CARAT DOESN'T NEED A KRAFT TYPE             
         BE    FFRST12                                                          
                                                                                
         LA    R5,KFTCTAB           TABLE OF KRAFT CLIENTS                      
KFRST11  CLI   0(R5),X'FF'          END OF TABLE                                
         BNE   *+6                                                              
*                                                                               
         DC    H'0'                 INVALID CLIENT ENCOUNTERED                  
*                                                                               
         CLC   KCLT(3),0(R5)                                                    
         BE    KFRST11C                                                         
         LA    R5,4(R5)             NEXT ENTRY                                  
         B     KFRST11                                                          
*                                                                               
KFRST11C MVC   KFTTYP,3(R5)         SET TYPE (G OR S)                           
*                                                                               
*        BE SURE THAT NO BINARY ZEROS REMAIN IN THESE FIELDS                    
*                                                                               
FFRST12  OC    KEBCAN,SPACES                                                    
         OC    KREQNUM,SPACES                                                   
         OC    KINV,SPACES                                                      
*                                                                               
FFRST20  DS    0H                                                               
         CLI   FIRSTINV,C'Y'                                                    
         BNE   KFRST25                                                          
*                                                                               
*        SEND FILE HEADER RECORD                                                
*                                                                               
         LA    R5,MVKRHDM                                                       
         BAS   RE,PROCTAB        GO PROCESS TABLE ENTRIES                       
*                                                                               
KFRST25  BAS   RE,KPROCESS       PROCESS THE RECORD                             
         LA    R5,MVKRIVM        INVOICE TABLE ENTRIES                          
         BAS   RE,PROCTAB                                                       
*                                                                               
         LA    R5,MVKRIDT2       ESTITMATE FOR KRAFT                            
         CLI   CARSW,C'N'                                                       
         BE    KFRST27                                                          
         LA    R5,MVKRIDT3       CARAT FORMAT                                   
KFRST27  BAS   RE,PROCTAB                                                       
*                                                                               
         LA    R5,MVKRIDT4       INVOICE TABLE ENTRIES                          
         BAS   RE,PROCTAB                                                       
*                                                                               
         L     R1,=A(DATAFILE)                                                  
         LA    R2,REC                                                           
         GET   (1),(2)                                                          
         CLC   REC(50),SPACES       MAY ENCOUNTER EMPTY RECORDS                 
         BE    DATAEND              JUST IGNORE - TRY GOING TO DATAEND          
         B     KFRST25                                                          
         DROP  R2                                                               
*                                                                               
NEWINV   NTR1                                                                   
         LA    R2,REC                                                           
         USING CRECD,R2                                                         
         CLI   FIRSTINV,C'Y'    FIRST INVOICE?                                  
         BNE   NEWINV20                                                         
*                                                                               
*        SEND FILE HEADER RECORD                                                
*                                                                               
         LA    R5,YNCVHDM                                                       
         BAS   RE,PROCTAB        GO PROCESS TABLE ENTRIES                       
*                                                                               
*        NEW INVOICE  - SEND HEADER RECORD                                      
*                                                                               
NEWINV20 DS    0H                                                               
         OC    LASTINV,LASTINV      DO I NEED TO FINISH LAST INV?               
         BZ    NEWINV25                                                         
*                                                                               
         MVI   CHVIAMT-1,X'FF'                                                  
         MVC   CHVIAMT,SPACES                                                   
         EDIT  (P8,INVDOLS),(13,CHVIAMT),2,ALIGN=LEFT,FLOAT=-                   
         LA    R1,CHVIAMT+L'CHVIAMT-1                                           
NEWINV21 CLI   0(R1),C' '                                                       
         BH    NEWINV22                                                         
         BCT   R1,NEWINV21                                                      
*                                                                               
NEWINV22 MVC   1(22,R1),=C'</pidx:MonetaryAmount>'                              
*                                                                               
*                                                                               
         CLI   YNASW,C'Y'                                                       
         BNE   NEWINV23                                                         
*                                ALTER SOME FIELDS                              
*                                BEFORE GOING TO PROCTAB                        
         MVI   CHVLDUE-1,X'FF'                                                  
         MVC   CHVLDUE,SPACES                                                   
         EDIT  (P8,INVDOLS),(13,CHVLDUE),2,ALIGN=LEFT,FLOAT=-                   
         LA    R1,CHVLDUE+L'CHVLDUE-1                                           
NINV21   CLI   0(R1),C' '                                                       
         BH    NINV22                                                           
         BCT   R1,NINV21                                                        
*                                                                               
NINV22   MVC   1(22,R1),=C'</pidx:MonetaryAmount>'                              
*                                                                               
         LA    R5,YNCVLNM        SPECIAL DETAIL ITEM                            
         BAS   RE,PROCTAB                                                       
*                                                                               
NEWINV23 LA    R5,YNCVTOM        END OF INVOICE TABLE                           
         BAS   RE,PROCTAB                                                       
         B     NEWINV24                                                         
*                                                                               
NEWINV24 AP    RUNINVS,=P'1'        BUMP RUN INVOICES                           
         AP    RUNDOLS,INVDOLS      ADD TO RUN TOTALS                           
*                                                                               
NEWINV25 ZAP   INVDOLS,=P'0'        CLEAR INVOICE TOTAL $                       
*                                                                               
*        SAVE DATA IN LWSD                                                      
*                                                                               
         MVC   CHVPLCD,CPLANTC      PLANT CODE                                  
         MVC   INVNUMB,SPACES                                                   
         MVC   INVNUMB(9),CINVNUM   INVOICE NUMBER                              
         MVC   CHVINVD,CBILLDTE     BILLED DATE                                 
         MVI   CHVSUPL-1,X'FF'    TO PREVENT GOING PAST BEGINNING               
         MVC   CHVSUPL,SPACES                                                   
         MVI   CHVURL-1,X'FF'     TO PREVENT GOING PAST BEGINNING               
         MVC   CHVURL,SPACES                                                    
**OLD**  MVC   CHVURL(36),=C'https://asp3.imageserv.com/WebVault/'              
         MVC   CHVURL(46),=C'https://asp3.imageserv.com/WebVaultNativeVX        
               iew/'                                                            
         CLC   CSUPPLR(10),=C'0050024200'    Y&R                                
         BE    NEWINV27                                                         
         CLC   CSUPPLR(10),=C'0050025128'    BRAVO                              
         BE    NEWINV27                                                         
         CLC   CSUPPLR(10),=C'0050064632'    WUNDERMAN                          
         BE    NEWINV27                                                         
         MVC   CHVURL,SPACES              NONE FOR OTHERS                       
NEWINV27 DS    0H                                                               
*                                                                               
         MVI   CHVFTR-1,X'FF'                                                   
         MVC   CHVFTR(100),SPACES                                               
         MVC   CHVFTR+100(100),SPACES                                           
*                                                                               
         LA    RE,L'CACCBOTH                                                    
         LA    R1,CACCBOTH                                                      
NEWINV26 CLI   0(R1),C'&&'      CHANGE & TO + (XML CAN'T HANDLE)                
         BNE   *+8                                                              
         MVI   0(R1),C'+'                                                       
         LA    R1,1(R1)                                                         
         BCT   RE,NEWINV26                                                      
*                                                                               
         MVC   CHVFTR(L'CACCBOTH),CACCBOTH                                      
*                                                                               
         LA    R1,CHVFTR+L'CHVFTR-1                                             
         CLI   0(R1),C' '     SCAN BACKWARD FOR FIRST NON-SPACE                 
         BH    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         MVC   1(25,R1),=C'</pidx:FieldTicketNumber>'                           
*                                                                               
         MVI   CHVSEA-1,X'FF'        SERVICE ENTRY APPROVER                     
         MVC   CHVSEA(100),SPACES    FROM CUSER1                                
         MVC   CHVSEA(L'CUSER1),CUSER1                                          
*                                                                               
         LA    R1,CHVSEA+L'CHVSEA-1                                             
         CLI   0(R1),C' '     SCAN BACKWARD FOR FIRST NON-SPACE                 
         BH    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         MVC   1(25,R1),=C'</pidx:ContactIdentifier>'                           
*                                                                               
         LA    R1,CHVURL+L'CHVURL-1                                             
         CLI   0(R1),C' '     SCAN BACKWARD FOR FIRST NON-SPACE                 
         BH    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         MVC   1(26,R1),=C'</pidx:AttachmentLocation>'                          
*                                                                               
                                                                                
         MVI   CHVPO#-1,X'FF'        SERVICE ENTRY APPROVER                     
         MVC   CHVPO#(60),SPACES    FROM CUSER2                                 
         MVC   CHVPO#(L'CUSER2),CUSER2                                          
*                                                                               
         LA    R1,CHVPO#+L'CHVPO#-1                                             
         CLI   0(R1),C' '     SCAN BACKWARD FOR FIRST NON-SPACE                 
         BH    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         MVC   1(27,R1),=C'</pidx:PurchaseOrderNumber>'                         
*                                                                               
         MVC   RUNSUPL(L'CSUPPLR),CSUPPLR    SAVE SUPPLIER ID                   
*                                                                               
         MVC   CHVSUPL(L'CSUPPLR),CSUPPLR                                       
         LA    R1,CHVSUPL+L'CHVSUPL-1                                           
YNCVIT2S CLI   0(R1),C' '                                                       
         BH    YNCVIT2X                                                         
         BCT   R1,YNCVIT2S         SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
YNCVIT2X MVC   1(25,R1),=C'</pidx:PartnerIdentifier>'                           
*                                                                               
         LA    R5,YNCVIVM  INVOICE HEADER START TABLE FOR FIRST                 
         CLI   FIRSTINV,C'Y'                                                    
         BE    *+8                                                              
         LA    R5,YNCVIVM2  INVOICE HEADER START TABLE FOR SUBSEQUENT           
         BAS   RE,PROCTAB   GO PROCESS TABLE ENTRIES                            
*                                                                               
         MVI   FIRSTINV,C'N' SET OFF FIRST INVOICE INDICATOR                    
*                                                                               
         LA    R5,YNCVIP    INVOICE PROPERTIES (FOR BOTH)                       
         BAS   RE,PROCTAB   GO PROCESS TABLE ENTRIES                            
*                                                                               
YNCVIX   XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*        PROCESS TABLE  - R5 POINTS TO IT                                       
******************************************************************              
PROCTAB  NTR1                                                                   
         XC    OUTREC(250),OUTREC                                               
         XC    OUTREC+250(150),OUTREC+250                                       
*                                                                               
PROCT2   CLI   0(R5),EOTQ        END OF TABLE                                   
         BE    PROCT20                                                          
*                                                                               
         CLI   0(R5),EORQ        END OF RECORD?                                 
         BNE   PROCT5                                                           
         MVC   OUTREC-4(2),1(R5) RECORD LENGTH                                  
*                                                                               
         CLI   SVQOPT2,C'Y'         TEST RUN?                                   
         BE    PROCT4             NO OUTPUT                                     
*                                                                               
         TM    FCS,FCSEDI          TEST XML FILE OPEN                           
         BO    PROCT3                                                           
*                                                                               
         CLI   CARSW,C'Y'         CARAT'S KRAFT                                 
         BNE   PROCT2NC                                                         
         MVC   MAPNME(20),=CL20'PRTTAPE.PP0AEUB'                                
         B     PROCT2K                                                          
*                                                                               
PROCT2NC CLI   KFTSW,C'Y'         KRAFT?                                        
         BNE   PROCT2NK                                                         
*                                                                               
         MVC   MAPNME(20),=CL20'PRTTAPE.PP0AEO01'                               
*                                  OR (PRTTAPE.PP0XMXX)                         
         MVI   MAPNME+15,C'2'                                                   
         CLI   KFTTYP,C'G'       GROCERY FILE                                   
         BE    PROCT2K                                                          
         MVI   MAPNME+15,C'3'                                                   
         CLI   KFTTYP,C'S'      SNACK FILE                                      
         BE    PROCT2K                                                          
         MVI   MAPNME+15,C'4'                                                   
         CLI   KFTTYP,C'K'      GROCERY FILE - CLT KRS                          
         BE    PROCT2K                                                          
         DC    H'0'             INVALID KRAFT FILE                              
*                                                                               
*                                                                               
PROCT2K  GOTO1 DYNALLOC,DMCB,(0,=C'XMLMAP  '),(0,MAPNME)                        
         OPEN  (XMLMAP,(OUTPUT))                                                
         B     PROCT2X                                                          
                                                                                
*                                                                               
*   HERE FOR NON-KRAFT FILES                                                    
*                                                                               
PROCT2NK MVI   BYTE,X'45'         X'04' = BIG NAMES                             
         MVC   DUB,=X'000005000001'                                             
         GOTO1 DYNALLOC,DMCB,(X'80',=C'XMLMAP  '),(BYTE,DUB),          X        
               (X'80',MQMAPNM)                                                  
*****    GOTO1 DYNALLOC,DMCB,(0,=C'XMLMAP  '),MQMAPNM                           
         OPEN  (XMLMAP,(OUTPUT))                                                
PROCT2X  LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         OI    FCS,FCSEDI                                                       
*                                                                               
PROCT3   DS    0H                                                               
         PUT   XMLMAP,OUTREC-4                                                  
*                                                                               
PROCT4   LA    R5,3(R5)          BUMP TO NEXT ENTRY                             
*                                CLEAR OUTPUT AREA                              
         XC    OUTREC(250),OUTREC                                               
         XC    OUTREC+250(150),OUTREC+250                                       
*                                                                               
         B     PROCT2                                                           
*                                                                               
PROCT5   CLI   0(R5),CONQ        CONSTANT                                       
         BNE   PROCT10                                                          
         LA    R6,OUTREC                                                        
         MVC   HALF,1(R5)        DISPLACEMENT INTO OUTREC                       
         AH    R6,HALF                                                          
         MVC   HALF,3(R5)        LENGTH OF DATA                                 
         LH    RE,HALF                                                          
         SH    RE,=H'1'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
*                                                                               
         MVC   0(0,R6),5(R5)                                                    
*                                                                               
         AH    R5,HALF           LENGTH OF CONSTANT DATA                        
         AH    R5,=H'5'          LENGTH OF ENTRY HEADER                         
         B     PROCT2                                                           
*                                                                               
PROCT10 CLI    0(R5),LWSQ        DATA FROM WORKING STORAGE?                     
         BNE   PROCT15                                                          
         LA    R6,OUTREC                                                        
         MVC   HALF,1(R5)        DISPLACEMENT INTO OUTREC                       
         AH    R6,HALF                                                          
         MVC   HALF,3(R5)        LENGTH OF DATA                                 
         SR    RF,RF                                                            
         ICM   RF,3,5(R5)          DISPLACEMENT INTO LWSD                       
         LA    RF,LWSD(RF)                                                      
         LH    RE,HALF                                                          
         SH    RE,=H'1'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(RF)                                                    
         AH    R5,=H'7'          LENGTH OF LWSD ENTRIES                         
         B     PROCT2                                                           
*                                                                               
PROCT15  DC     H'0'              BAD ENTRY HEADER                              
PROCT20  XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        PROCESS THE RECORD                                                     
*                                                                               
PROCESS  NTR1                                                                   
*                                                                               
         LA    R2,REC              ESTABLISH RECORD FROM ACC                    
         USING CRECD,R2                                                         
*                                                                               
*        SET DATA IN LWSD SO TABLE CAN PROCESS IT                               
*                                                                               
         MVI   CHVLDUE-1,X'FF'                                                  
         MVC   CHVLDUE,SPACES                                                   
         MVI   CHVDESC-1,X'FF'    TO PREVENT GOING PAST BEGINNING               
         MVC   CHVDESC(100),SPACES                                              
         MVC   CHVDESC+100(100),SPACES                                          
         MVI   CHVITEM-1,X'FF'    TO PREVENT GOING PAST BEGINNING               
         MVC   CHVITEM,SPACES                                                   
         MVI   CHVLITEM-1,X'FF'   TO PREVENT GOING PAST BEGINNING               
         MVC   CHVLITEM,SPACES                                                  
*                                                                               
         CLI   YNASW,C'Y'          ONE ITEM FOR YN                              
         BNE   YNCVD4                                                           
         MVC   CCOUNT,=C'   1'     ALWAYS ONE                                   
*                                                                               
YNCVD4   MVC   CHVITEM(L'CCOUNT),CCOUNT                                         
         LA    R1,CHVITEM+L'CHVITEM-1                                           
YNCVD7   CLI   0(R1),C' '                                                       
         BH    YNCVD8                                                           
         BCT   R1,YNCVD7           SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
YNCVD8   MVC   1(22,R1),=C'</pidx:LineItemNumber>'                              
*                                                                               
         MVC   CHVLITEM(L'CCOUNT),CCOUNT                                        
         LA    R1,CHVLITEM+L'CHVLITEM-1                                         
YNCVD8B  CLI   0(R1),C' '                                                       
         BH    YNCVD8C                                                          
         BCT   R1,YNCVD8B          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
YNCVD8C  MVC   1(22,R1),=C'</pidx:TotalLineItems>'                              
*                                                                               
         MVC   CHVLDUE(L'CNETBILL),CNETBILL                                     
         LA    R1,CHVLDUE+L'CHVLDUE-1                                           
YNCVD9   CLI   0(R1),C' '                                                       
         BH    YNCVD10                                                          
         BCT   R1,YNCVD9           SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
YNCVD10  MVC   1(22,R1),=C'</pidx:MonetaryAmount>'                              
*                                                                               
         MVI   CHVDESC-1,X'FF'                                                  
         MVC   CHVDESC(100),SPACES      LINE ITEM NAME                          
         MVC   CHVDESC+100(100),SPACES                                          
*                                                                               
         LA    RE,L'CCONTRA                                                     
         LA    R1,CCONTRA                                                       
YNCVD10B CLI   0(R1),C'&&'                                                      
         BNE   *+8                                                              
         MVI   0(R1),C'+'   CHANGE & TO + (XML CAN'T HANDLE &)                  
         LA    R1,1(R1)                                                         
         BCT   RE,YNCVD10B                                                      
*                                                                               
         LA    RE,L'CWKTSK                                                      
         LA    R1,CWKTSK                                                        
YNCVD10C CLI   0(R1),C'&&'                                                      
         BNE   *+8                                                              
         MVI   0(R1),C'+'   CHANGE & TO + (XML CAN'T HANDLE &)                  
         LA    R1,1(R1)                                                         
         BCT   RE,YNCVD10C                                                      
*                                                                               
         LA    RE,L'CREF                                                        
         LA    R1,CREF                                                          
YNCVD10D CLI   0(R1),C'&&'                                                      
         BNE   *+8                                                              
         MVI   0(R1),C'+'   CHANGE & TO + (XML CAN'T HANDLE &)                  
         LA    R1,1(R1)                                                         
         BCT   RE,YNCVD10D                                                      
*                                                                               
*                                                                               
*                                                                               
         CLI   YNASW,C'Y'          USE SPECIAL HARD CODE HERE                   
         BNE   YNCVD10M                                                         
         MVC   CHVDESC(23),=C'Total Billing Invoice #'                          
         MVC   CHVDESC+24(9),CINVNUM                                            
         MVC   CHVDESC+34(34),=C'See Attachment for Invoice Details'            
         LA    R1,CHVDESC+67    LOOKS WRONG BUT IS RIGHT                        
         B     YNCVD16                                                          
*                                                                               
YNCVD10M MVC   CHVDESC(L'CCONTRA),CCONTRA                                       
         LA    R1,CHVDESC+L'CHVDESC-1                                           
YNCVD11  CLI   0(R1),C' '                                                       
         BH    YNCVD12                                                          
         BCT   R1,YNCVD11          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
YNCVD12  MVI   2(R1),C'/'                                                       
         MVC   3(L'CWKTSK-3,R1),CWKTSK+3 (PAST WORKCODE)                        
         LA    R1,L'CWKTSK(R1)    -3 +3 = 0                                     
YNCVD13  CLI   0(R1),C' '                                                       
         BH    YNCVD14                                                          
         BCT   R1,YNCVD13          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
YNCVD14  MVI   2(R1),C'/'                                                       
         MVC   3(L'CREF,R1),CREF                                                
         LA    R1,L'CREF+3(R1)                                                  
YNCVD15  CLI   0(R1),C' '                                                       
         BH    YNCVD16                                                          
         BCT   R1,YNCVD15          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
YNCVD16  MVC   1(27,R1),=C'</pidx:LineItemDescription>'                         
*                                                                               
*                                                                               
         MVC   CHVWORKC,SPACES     FIRST 2 BYTES NOW IS WORKCODE                
         MVC   CHVWORKC(2),CWKTSK                                               
         LA    R1,CHVWORKC+2                                                    
         CLI   YNASW,C'Y'      SPECIAL ARIBA FORMAT?                            
         BNE   YNCVD16C         NO                                              
         MVC   CHVWORKC(15),=C'Summary Invoice'                                 
         LA    R1,CHVWORKC+15                                                   
*                                                                               
YNCVD16C MVC   0(26,R1),=C'</pidx:LineItemIdentifier>'                          
*                                                                               
         CLI   YNASW,C'Y'      SEE IF SKIPING LINE ITEM DETAILS                 
         BE    YNCVD16X                                                         
*                                                                               
         LA    R5,YNCVLNM                                                       
         BAS   RE,PROCTAB                                                       
*                                                                               
*        ADD LINE ITEM $ TO INVOICE TOTAL                                       
*                                                                               
YNCVD16X MVC   WORK(40),SPACES                                                  
         MVC   WORK(L'CNETBILL),CNETBILL                                        
         LA    R1,WORK                                                          
         LA    R6,13    LENGTH OF CNETBILL                                      
YNCVD17  CLI   0(R1),C' '     FIND FIRST NON-SPACE                              
         BH    YNCVD18                                                          
         LA    R1,1(R1)                                                         
         SH    R6,=H'1'                                                         
         B     YNCVD17                                                          
*                                                                               
YNCVD18  MVC   WORK+20(L'CNETBILL),0(R1)                                        
         GOTO1 VCASHVAL,DMCB,(X'80',WORK+20),(R6)                               
         AP    INVDOLS,DMCB+4(8)                                                
                                                                                
         CLI   RCWRITE,C'Y'        TEST WRITE = NO                              
         BNE   PROCPUTX                                                         
*                                                                               
         GOTO1 PUTPRT                                                           
*                                                                               
PROCPUTX DS    0H                                                               
*                                                                               
         B     PROCX                                                            
*                                                                               
PROCX    XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*        PROCESS THE KRAFT RECORD                                               
*                                                                               
KPROCESS NTR1                                                                   
*                                                                               
         LA    R2,REC              ESTABLISH RECORD FROM ACC                    
         USING KRECD,R2                                                         
*                                                                               
         AP    RUNINVS,=P'1'                                                    
         MVC   WORK(40),SPACES                                                  
         MVC   WORK(L'KINVAMT),KINVAMT                                          
         LA    R1,WORK                                                          
         LA    R6,L'KINVAMT                                                     
KP17     CLI   0(R1),C' '     FIND FIRST NON-SPACE                              
         BH    KP18                                                             
         LA    R1,1(R1)                                                         
         SH    R6,=H'1'                                                         
         B     KP17                                                             
*                                                                               
KP18     MVC   WORK+20(L'KINVAMT),0(R1)                                         
         GOTO1 VCASHVAL,DMCB,(X'80',WORK+20),(R6)                               
         AP    RUNINVD,DMCB+4(8)                                                
*                                                                               
         MVC   WORK(40),SPACES                                                  
         MVC   WORK(L'KTAX),KTAX                                                
         LA    R1,WORK                                                          
         LA    R6,L'KTAX                                                        
KP19     CLI   0(R1),C' '     FIND FIRST NON-SPACE                              
         BH    KP20                                                             
         LA    R1,1(R1)                                                         
         SH    R6,=H'1'                                                         
         B     KP19                                                             
*                                                                               
KP20     MVC   WORK+20(L'KTAX),0(R1)                                            
         GOTO1 VCASHVAL,DMCB,(X'80',WORK+20),(R6)                               
         AP    RUNCTAX,DMCB+4(8)                                                
*                                                                               
         MVC   WORK(40),SPACES                                                  
         MVC   WORK(L'KAMTDUE),KAMTDUE                                          
         LA    R1,WORK                                                          
         LA    R6,L'KAMTDUE                                                     
KP21     CLI   0(R1),C' '     FIND FIRST NON-SPACE                              
         BH    KP22                                                             
         LA    R1,1(R1)                                                         
         SH    R6,=H'1'                                                         
         B     KP21                                                             
*                                                                               
KP22     MVC   WORK+20(L'KAMTDUE),0(R1)                                         
         GOTO1 VCASHVAL,DMCB,(X'80',WORK+20),(R6)                               
         AP    RUNDOLS,DMCB+4(8)                                                
*                                                                               
*        SET DATA IN LWSD SO TABLE CAN PROCESS IT                               
*                                                                               
         MVI   MVKDDUE-1,X'FF'     AMOUNT                                       
         MVC   MVKDDUE,SPACES                                                   
         MVI   MVKTAX-1,X'FF'      TAX                                          
         MVC   MVKTAX,SPACES                                                    
         MVI   MVKTDUE-1,X'FF'     DUE                                          
         MVC   MVKTDUE,SPACES                                                   
         MVI   KFTEBC-1,X'FF'      EBCANUMBER                                   
         MVC   KFTEBC,SPACES                                                    
*                                                                               
         MVC   KFTEST,KEST                                                      
         MVC   KFTESTN,KESTNAME                                                 
         MVC   KFTINV,KINV                                                      
         MVC   KFTIDTE,KINVDTE                                                  
         MVC   KFTCLT,KCLT                                                      
         MVC   KFTPRD,KPRD                                                      
         MVC   KFTCUR,KCUR                                                      
*                                                                               
         MVC   MVKDDUE(L'KINVAMT),KINVAMT                                       
         LA    R1,MVKDDUE+L'MVKDDUE-1                                           
KPROC5   CLI   0(R1),C' '                                                       
         BH    KPROC10                                                          
         BCT   R1,KPROC5           SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
KPROC10  MVC   1(12,R1),=C'</InvAmount>'                                        
*                                                                               
         MVC   MVKTAX(L'KTAX),KTAX                                              
         LA    R1,MVKTAX+L'MVKTAX-1                                             
KPROC15  CLI   0(R1),C' '                                                       
         BH    KPROC20                                                          
         BCT   R1,KPROC15          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
KPROC20  MVC   1(10,R1),=C'</TAX-HST>'                                          
*                                                                               
         MVC   MVKTDUE(L'KAMTDUE),KAMTDUE                                       
         LA    R1,MVKTDUE+L'MVKTDUE-1                                           
KPROC25  CLI   0(R1),C' '                                                       
         BH    KPROC30                                                          
         BCT   R1,KPROC25          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
KPROC30  MVC   1(12,R1),=C'</AmountDue>'                                        
*                                                                               
         MVC   KFTEBC(L'KEBCAN),KEBCAN                                          
         LA    R1,KFTEBC+L'KFTEBC-1                                             
KPROC35  CLI   0(R1),C' '                                                       
         BH    KPROC40                                                          
         BCT   R1,KPROC35          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
KPROC40  MVC   1(13,R1),=C'</eBCANumber>'                                       
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*        PROCESS RUN LAST                                                       
*                                                                               
LAST     DS    0H                                                               
         CLI   KFTSW,C'Y'          KRAFT FORMAT?                                
         BE    KLAST                                                            
         CLI   CARSW,C'Y'          CARAT FORMAT?                                
         BE    KLAST                                                            
         OC    LASTINV,LASTINV     DID I PROCESS ANYTHING?                      
         BZ    XIT                                                              
*                                                                               
*       FINISH LAST INVOICE        CHEVRON FILES ONLY                           
*                                                                               
         MVI   CHVIAMT-1,X'FF'                                                  
         MVC   CHVIAMT,SPACES                                                   
         EDIT  (P8,INVDOLS),(13,CHVIAMT),2,ALIGN=LEFT,FLOAT=-                   
         LA    R1,CHVIAMT+L'CHVIAMT-1                                           
LASINV21 CLI   0(R1),C' '                                                       
         BH    LASINV22                                                         
         BCT   R1,LASINV21                                                      
*                                                                               
LASINV22 MVC   1(22,R1),=C'</pidx:MonetaryAmount>'                              
*                                                                               
*                                                                               
         CLI   YNASW,C'Y'                                                       
         BNE   LASINV23                                                         
*                                ALTER SOME FIELDS                              
*                                BEFORE GOING TO PROCTAB                        
         MVI   CHVLDUE-1,X'FF'                                                  
         MVC   CHVLDUE,SPACES                                                   
         EDIT  (P8,INVDOLS),(13,CHVLDUE),2,ALIGN=LEFT,FLOAT=-                   
         LA    R1,CHVLDUE+L'CHVLDUE-1                                           
LINV21   CLI   0(R1),C' '                                                       
         BH    LINV22                                                           
         BCT   R1,LINV21                                                        
*                                                                               
LINV22   MVC   1(22,R1),=C'</pidx:MonetaryAmount>'                              
*                                                                               
         LA    R5,YNCVLNM        SPECIAL DETAIL ITEM                            
         BAS   RE,PROCTAB                                                       
*                                                                               
LASINV23 LA    R5,YNCVTOM        END OF INVOICE TABLE                           
         BAS   RE,PROCTAB        GO PROCESS TABLE ENTRIES                       
*                                                                               
         AP    RUNINVS,=P'1'        BUMP RUN INVOICES                           
         AP    RUNDOLS,INVDOLS      ADD TO RUN TOTALS                           
*                                                                               
         LA    R5,YNCVEND         END OF XML FILE STUFF                         
         BAS   RE,PROCTAB                                                       
*                                                                               
         MVC   P1+3(L'RUNSUPL),RUNSUPL                                          
         EDIT  (P8,RUNINVS),(8,P1+20),0,ALIGN=LEFT                              
         EDIT  (P8,RUNDOLS),(13,P1+35),2,COMMAS=YES,ALIGN=LEFT,FLOAT=-          
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         B     LASTALL                                                          
         EJECT                                                                  
*                                                                               
KLAST    DS    0H           END OF KRAFT PROCESSING                             
         LA    R5,MVKREND                                                       
         BAS   RE,PROCTAB                                                       
         EDIT  (P8,RUNINVS),(8,P1+20),0,ALIGN=LEFT                              
         EDIT  (P8,RUNINVD),(13,P1+35),2,COMMAS=YES,ALIGN=LEFT,FLOAT=-          
         EDIT  (P8,RUNCTAX),(13,P1+50),2,COMMAS=YES,ALIGN=LEFT,FLOAT=-          
         EDIT  (P8,RUNDOLS),(13,P1+65),2,COMMAS=YES,ALIGN=LEFT,FLOAT=-          
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,2                                                       
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         B     LASTALLK                                                         
         EJECT                                                                  
*                                                                               
*        THE ROUTINE FOR ALL FILE FORMATS                                       
*                                                                               
LASTALL  MVC   P1+3(L'MQMAPNM+L'DSNAME),MQMAPNM     DISPLAY FILE NAME           
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
LASTALLK TM    FCS,FCSEDI          OUTPUT FILE OPEN?                            
         BZ    LAST5                                                            
         CLOSE (XMLMAP)                                                         
         LTR   RF,RF                                                            
         BZ    LAST2                                                            
         DC    H'0'                                                             
*                                                                               
LAST2    DS    0H                                                               
         CLI   TESTMQ,C'N'      DON'T DO MQ RECORD                              
         BE    XIT                                                              
         CLI   TESTMQ,C' '      OR SPACE                                        
         BE    XIT                                                              
*                                                                               
* SEND MQ MESSAGE WITH FILE NAME                                                
         LA    R5,ELEM                                                          
         USING MQMSGD,R5                                                        
         MVI   ELEM,C' '                                                        
         MVC   ELEM+1(MQMSGLNQ-1),ELEM                                          
         LA    R1,MQMAPNM                                                       
         MVC   MQFILE(34),14(R1)   BIL.SYS.AGID.DYYMMDD.THHMMSS                 
*                                  SYS=SYSTEM,AGID= 4 CHARACTER AGY ID          
*                          14(R1) TO GET PAST SPTPDISK.PROD (OR TEST)           
*                                                                               
         CLI   SVQOPT5,C'X'        ALTER NAME TO ONE THAT WON'T EXIST           
         BNE   *+10                                                             
         MVC   MQFILE+13(15),=C'D000000.T000000'                                
*                                                                               
         MVC   MQDATE(6),28(R1)    YYMMDD OF FILE NAME                          
         MVC   MQTIME(6),36(R1)    HHMMSS OF FILE NAME                          
*                                                                               
         BRAS  RE,MQOPEN                                                        
*                                                                               
         MVC   MQHID,=CL6'DANOT1'                                               
         MVC   MQSYS,MQFILE+4             SYSTEM (+4 PAST BIL.)                 
         MVC   MQAGYID,MQFILE+8           AGENCY 1D (4 CHAR)                    
         MVC   MQQUAL(7),=C'BILLING'                                            
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'PUT'),ELEM,MQMSGLNQ,0                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
         CLI   DMCB+8,0                                                         
         BE    XIT                                                              
         DCHO                                                                   
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
LAST5    CLI   OPENPQ,C'Y'                                                      
         BNE   XIT                                                              
         GOTO1 PRINT,DMCB,=C'CLOSE'                                             
         B     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
KFTCTAB  DC    C'KGM',C'G'        CLT CODE, TYPE (G OR S)                       
         DC    C'MDM',C'S'                                                      
         DC    C'KRS',C'K'        KRS - GROCERY                                 
         DC    X'FFFFFF'          END OF TABLE                                  
*                                                                               
         DS    0D                                                               
         EJECT                                                                  
MQOPEN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   DMCB+8,X'A0'        SUPPRESS LENGTH FOR MESSAGE & HDR            
*                                                                               
* IF WE'RE RUNNING A TEST, SEND TO TEST MQ BROKER                               
         CLI   TESTMQ,C'T'         IS THIS A MQ TEST RUN                        
         BNE   *+8                  NO                                          
         OI    DMCB+8,X'01'         YES -PUT TO TEST MQ BROKER                  
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'OPEN'),(0,=C'MEDIACOMSFTP****'),,0             
         CLI   DMCB+8,0                                                         
         JE    XIT                                                              
         DCHO                                                                   
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*********************************************************************           
* YNRO CHEVRON  XML MAP TABLE(S) - SEE MAPD                                     
*********************************************************************           
*                                  FILE HEADER                                  
YNCVHDM  DS    0X                  YNRO - CHEVRON VERSION 4010                  
*                                                                               
         DC    AL1(CONQ),AL2(0,39),C'<?xml version="1.0" encoding="UTF-X        
               8" ?>'                                                           
         DC    AL1(EORQ),AL2(4+39)                                              
*                                                                               
**OLD    DC    AL1(CONQ),AL2(0,63),C'<pidx:DataObjects xmlns:pidx="http         
**OLD          ://www.api.org/pidxXML/v1.0">'                                   
**OLD    DC    AL1(EORQ),AL2(4+63)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,89),C'<dds:DataObjects xmlns:dds="http:/X        
               /www.dds.net/schemas" xmlns:xsi="http://www.w3.org/2001'         
         DC    AL1(CONQ),AL2(89,93),C'/XMLSchema-instance" xsi:schemaLoX        
               cation="http://www.dds.net/schemas generic-wrapper-1.0.xX        
               sd">'                                                            
         DC    AL1(EORQ),AL2(4+89+93)                                           
*                                                                               
*                                                                               
         DC    AL1(EOTQ,EOTQ)                                                   
**                                                                              
**  END OF FILE HEADER DATA                                                     
**                                                                              
YNCVIVM  DS    0X    INVOICE HEADER START - FOR FIRST INVOICE                   
*                                                                               
**OLD    DC    AL1(CONQ),AL2(0,77),C'<pidx:Invoice pidx:version="1.0" p         
**OLD          idx:transactionPurposeIndicator="Original">'                     
**OLD    DC    AL1(EORQ),AL2(4+77)                                              
*                                                                               
**OLD2   DC    AL1(CONQ),AL2(0,77),C'<pidx:Invoice xmlns:pidx="http://w         
**OLD2         ww.api.org/pidXML/v1.0" xsi:schemaLocationx'                     
**OLD2   DC    AL1(CONQ),AL2(77,67),C'"http://www.api.org/pidXML/v1.0 I         
**OLD2         nvoice-2002-02-14-v1-0-Merge.xsd" '                              
**OLD2   DC    AL1(CONQ),AL2(144,63),C'pidx:version="1.0" pidx:transact         
**OLD2         ionPurposeIndicator="Original">'                                 
**OLD2   DC    AL1(EORQ),AL2(4+77+67+63)                                        
*                                                                               
         DC    AL1(CONQ),AL2(0,58),C'<pidx:Invoice xmlns:pidx="http://wX        
               ww.api.org/pidXML/v1.0" '                                        
         DC    AL1(CONQ),AL2(58,86),C'xsi:schemaLocation="http://www.apX        
               i.org/pidXML/v1.0 Invoice-2002-02-14-V1-0-Merge.xsd" '           
         DC    AL1(CONQ),AL2(144,63),C'pidx:version="1.0" pidx:transactX        
               ionPurposeIndicator="Original">'                                 
         DC    AL1(EORQ),AL2(4+77+67+63)                                        
*                                                                               
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
*               INVOICE HEADER START FOR SUBSEQUENT INVOICES                    
*                                                                               
YNCVIVM2 DS    0X                                                               
         DC    AL1(CONQ),AL2(0,58),C'<pidx:Invoice xmlns:pidx="http://wX        
               ww.api.org/pidXML/v1.0" '                                        
         DC    AL1(CONQ),AL2(058,63),C'pidx:version="1.0" pidx:transactX        
               ionPurposeIndicator="Original">'                                 
         DC    AL1(EORQ),AL2(4+58+63)                                           
*                                                                               
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
YNCVIP   DC    AL1(CONQ),AL2(0,24),C'<pidx:InvoiceProperties>'                  
         DC    AL1(EORQ),AL2(4+24)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,20),C'<pidx:InvoiceNumber>'                      
         DC    AL1(LWSQ),AL2(20,L'INVNUMB,INVNUMB-LWSD)                         
         DC    AL1(CONQ),AL2(20+L'INVNUMB,21),C'</pidx:InvoiceNumber>'          
         DC    AL1(EORQ),AL2(4+20+L'INVNUMB+21)                                 
*                                                                               
         DC    AL1(CONQ),AL2(0,18),C'<pidx:InvoiceDate>'                        
         DC    AL1(LWSQ),AL2(18,L'CHVINVD,CHVINVD-LWSD)                         
         DC    AL1(CONQ),AL2(18+L'CHVINVD,19),C'</pidx:InvoiceDate>'            
         DC    AL1(EORQ),AL2(4+18+L'CHVINVD+19)                                 
*                                                                               
         DC    AL1(CONQ),AL2(0,17),C'<pidx:Attachment>'                         
         DC    AL1(EORQ),AL2(4+17)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,25),C'<pidx:AttachmentLocation>'                 
         DC    AL1(LWSQ),AL2(25,L'CHVURL,CHVURL-LWSD)                           
         DC    AL1(EORQ),AL2(4+25+L'CHVURL)                                     
*                                                                               
         DC    AL1(CONQ),AL2(0,18),C'</pidx:Attachment>'                        
         DC    AL1(EORQ),AL2(4+18)                                              
*                                                                               
*                                                                               
         DC    AL1(CONQ),AL2(0,55),C'<pidx:PartnerInformation partnerRoX        
               leIndicator="Seller">'                                           
         DC    AL1(EORQ),AL2(4+55)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,24),C'<pidx:PartnerIdentifier '                  
         DC    AL1(CONQ),AL2(24,45),C'partnerIdentifierIndicator="AssigX        
               nedByBuyer">'                                                    
         DC    AL1(LWSQ),AL2(24+45,L'CHVSUPL,CHVSUPL-LWSD)                      
         DC    AL1(EORQ),AL2(4+24+45+L'CHVSUPL)                                 
*                                                                               
*****    DC    AL1(CONQ),AL2(0,25),C'<pidx:ContactInformation '                 
*****    DC    AL1(CONQ),AL2(25,51),C'contactInformationIndicator="Offi         
*****          ceRepresentative">'                                              
*****    DC    AL1(EORQ),AL2(4+25+51)                                           
*****                                                                           
*****    DC    AL1(CONQ),AL2(0,19),C'<pidx:EmailAddress>'                       
*****    DC    AL1(CONQ),AL2(19,16),C'ERROR EMAIL ADDR'                         
*****                                                                           
*****    DC    AL1(CONQ),AL2(31,20),C'</pidx:EmailAddress>'                     
*****    DC    AL1(EORQ),AL2(4+19+16+20)                                        
*****                                                                           
*****    DC    AL1(CONQ),AL2(0,26),C'</pidx:ContactInformation>'                
*****    DC    AL1(EORQ),AL2(4+26)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,26),C'</pidx:PartnerInformation>'                
         DC    AL1(EORQ),AL2(4+26)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,55),C'<pidx:PartnerInformation partnerRoX        
               leIndicator="SoldTo">'                                           
         DC    AL1(EORQ),AL2(4+55)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,24),C'<pidx:PartnerIdentifier '                  
         DC    AL1(CONQ),AL2(24,46),C'partnerIdentifierIndicator="AssigX        
               nedBySeller">'                                                   
         DC    AL1(CONQ),AL2(70,26),C' </pidx:PartnerIdentifier>'               
         DC    AL1(EORQ),AL2(4+24+46+26)                                        
*                                                                               
         DC    AL1(CONQ),AL2(0,25),C'<pidx:ContactInformation '                 
         DC    AL1(CONQ),AL2(25,46),C'contactInformationIndicator="BuyeX        
               rDepartment">'                                                   
         DC    AL1(EORQ),AL2(4+25+46)                                           
*                                                                               
         DC    AL1(CONQ),AL2(0,24),C'<pidx:ContactIdentifier '                  
         DC    AL1(CONQ),AL2(24,40),C'contactIdentifierIndicator="EmploX        
               yeeID">'                                                         
         DC    AL1(LWSQ),AL2(24+40,L'CHVSEA,CHVSEA-LWSD)                        
         DC    AL1(EORQ),AL2(4+24+40+L'CHVSEA)                                  
         DC    AL1(CONQ),AL2(0,26),C'</pidx:ContactInformation>'                
         DC    AL1(EORQ),AL2(4+26)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,26),C'</pidx:PartnerInformation>'                
         DC    AL1(EORQ),AL2(4+26)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,31),C'<pidx:PurchaseOrderInformation>'           
         DC    AL1(EORQ),AL2(4+31)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,26),C'<pidx:PurchaseOrderNumber>'                
         DC    AL1(LWSQ),AL2(26,L'CHVPO#,CHVPO#-LWSD)                           
         DC    AL1(EORQ),AL2(4+26+L'CHVPO#)                                     
*                                                                               
*                                                                               
         DC    AL1(CONQ),AL2(0,32),C'</pidx:PurchaseOrderInformation>'          
         DC    AL1(EORQ),AL2(4+32)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,29),C'<pidx:FieldTicketInformation>'             
         DC    AL1(EORQ),AL2(4+29)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,24),C'<pidx:FieldTicketNumber>'                  
         DC    AL1(LWSQ),AL2(24,L'CHVFTR,CHVFTR-LWSD)                           
         DC    AL1(EORQ),AL2(4+24+L'CHVFTR)                                     
*                                                                               
         DC    AL1(CONQ),AL2(0,30),C'</pidx:FieldTicketInformation>'            
         DC    AL1(EORQ),AL2(4+30)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,29),C'<pidx:JobLocationInformation>'             
         DC    AL1(EORQ),AL2(4+29)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,67),C'<pidx:JobLocationIdentifier jobLocX        
               ationIdentifierIndicator="Other">'                               
         DC    AL1(LWSQ),AL2(67,L'CHVPLCD,CHVPLCD-LWSD)                         
*                                                                               
         DC    AL1(CONQ),AL2(67+L'CHVPLCD,29),C'</pidx:JobLocationIdentX        
               ifier>'                                                          
         DC    AL1(EORQ),AL2(4+67+L'CHVPLCD+29)                                 
*                                                                               
         DC    AL1(CONQ),AL2(0,30),C'</pidx:JobLocationInformation>'            
         DC    AL1(EORQ),AL2(4+30)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,61),C'<pidx:ServiceDateTime dateTypeIndiX        
               cator="ServicePeriodStart">'                                     
         DC    AL1(LWSQ),AL2(61,L'CHVINVD,CHVINVD-LWSD)                         
         DC    AL1(CONQ),AL2(61+L'CHVINVD,9),C'T12:00:00'                       
         DC    AL1(CONQ),AL2(61+L'CHVINVD+9,23),C'</pidx:ServiceDateTimX        
               e>'                                                              
         DC    AL1(EORQ),AL2(4+61+L'CHVINVD+9+23)                               
*                                                                               
         DC    AL1(CONQ),AL2(0,59),C'<pidx:ServiceDateTime dateTypeIndiX        
               cator="ServicePeriodEnd">'                                       
         DC    AL1(LWSQ),AL2(59,L'CHVINVD,CHVINVD-LWSD)                         
         DC    AL1(CONQ),AL2(59+L'CHVINVD,9),C'T12:00:00'                       
         DC    AL1(CONQ),AL2(59+L'CHVINVD+9,23),C'</pidx:ServiceDateTimX        
               e>'                                                              
         DC    AL1(EORQ),AL2(4+59+L'CHVINVD+9+23)                               
*                                                                               
         DC    AL1(CONQ),AL2(0,25),C'</pidx:InvoiceProperties>'                 
         DC    AL1(EORQ),AL2(4+25)                                              
*                                                                               
*        INVOICE DETAIL HEADER (NOT PART OF LOOP)                               
*                                                                               
         DC    AL1(CONQ),AL2(0,21),C'<pidx:InvoiceDetails>'                     
         DC    AL1(EORQ),AL2(4+21)                                              
*                                                                               
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
*        INVOICE DETAILS (LOOP)                                                 
*                                                                               
YNCVLNM  DS    0X                                                               
         DC    AL1(CONQ),AL2(0,22),C'<pidx:InvoiceLineItem>'                    
         DC    AL1(EORQ),AL2(4+22)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,21),C'<pidx:LineItemNumber>'                     
         DC    AL1(LWSQ),AL2(21,L'CHVITEM,CHVITEM-LWSD)                         
         DC    AL1(EORQ),AL2(4+21+L'CHVITEM)                                    
*                                                                               
         DC    AL1(CONQ),AL2(0,22),C'<pidx:InvoiceQuantity>'                    
         DC    AL1(CONQ),AL2(22,15),C'<pidx:Quantity>'                          
         DC    AL1(CONQ),AL2(22+15,1),C'1'                                      
         DC    AL1(CONQ),AL2(22+15+1,16),C'</pidx:Quantity>'                    
         DC    AL1(EORQ),AL2(4+22+15+1+16)                                      
*                                                                               
         DC    AL1(CONQ),AL2(0,24),C'<pidx:UnitOfMeasureCode>'                  
         DC    AL1(CONQ),AL2(24,2),C'EA'                                        
         DC    AL1(CONQ),AL2(26,25),C'</pidx:UnitOfMeasureCode>'                
         DC    AL1(EORQ),AL2(4+24+2+25)                                         
*                                                                               
         DC    AL1(CONQ),AL2(0,23),C'</pidx:InvoiceQuantity>'                   
         DC    AL1(EORQ),AL2(4+23)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,26),C'<pidx:LineItemInformation>'                
         DC    AL1(EORQ),AL2(4+26)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,64),C'<pidx:LineItemIdentifier identifieX        
               rIndicator="AssignedBySeller">'                                  
         DC    AL1(LWSQ),AL2(64,L'CHVWORKC,CHVWORKC-LWSD)                       
         DC    AL1(EORQ),AL2(4+64+L'CHVWORKC)                                   
*                                                                               
         DC    AL1(CONQ),AL2(0,26),C'<pidx:LineItemDescription>'                
         DC    AL1(LWSQ),AL2(26,L'CHVDESC,CHVDESC-LWSD)                         
         DC    AL1(EORQ),AL2(4+26+L'CHVDESC)                                    
*                                                                               
         DC    AL1(CONQ),AL2(0,27),C'</pidx:LineItemInformation>'               
         DC    AL1(EORQ),AL2(4+27)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,14),C'<pidx:Pricing>'                            
         DC    AL1(EORQ),AL2(4+14)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,16),C'<pidx:UnitPrice>'                          
         DC    AL1(EORQ),AL2(4+16)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,21),C'<pidx:MonetaryAmount>'                     
         DC    AL1(LWSQ),AL2(21,L'CHVLDUE,CHVLDUE-LWSD)                         
         DC    AL1(EORQ),AL2(4+21+L'CHVLDUE)                                    
*                                                                               
         DC    AL1(CONQ),AL2(0,24),C'<pidx:UnitOfMeasureCode>'                  
         DC    AL1(CONQ),AL2(24,2),C'EA'                                        
         DC    AL1(CONQ),AL2(26,25),C'</pidx:UnitOfMeasureCode>'                
         DC    AL1(EORQ),AL2(4+24+2+25)                                         
*                                                                               
         DC    AL1(CONQ),AL2(0,19),C'<pidx:CurrencyCode>'                       
         DC    AL1(CONQ),AL2(19,3),C'USD'                                       
         DC    AL1(CONQ),AL2(22,20),C'</pidx:CurrencyCode>'                     
         DC    AL1(EORQ),AL2(4+19+3+20)                                         
         DC    AL1(CONQ),AL2(0,17),C'</pidx:UnitPrice>'                         
         DC    AL1(EORQ),AL2(4+17)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,15),C'</pidx:Pricing>'                           
         DC    AL1(EORQ),AL2(4+15)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,10),C'<pidx:Tax>'                                
         DC    AL1(EORQ),AL2(4+10)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,18),C'<pidx:TaxTypeCode>'                        
         DC    AL1(CONQ),AL2(18,5),C'Other'                                     
         DC    AL1(CONQ),AL2(18+5,19),C'</pidx:TaxTypeCode>'                    
         DC    AL1(EORQ),AL2(4+18+5+19)                                         
*                                                                               
         DC    AL1(CONQ),AL2(0,16),C'<pidx:TaxAmount>'                          
         DC    AL1(EORQ),AL2(4+16)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,21),C'<pidx:MonetaryAmount>'                     
         DC    AL1(CONQ),AL2(21,4),C'0.00'                                      
         DC    AL1(CONQ),AL2(21+4,22),C'</pidx:MonetaryAmount>'                 
         DC    AL1(EORQ),AL2(4+21+4+22)                                         
*                                                                               
         DC    AL1(CONQ),AL2(0,19),C'<pidx:CurrencyCode>'                       
         DC    AL1(CONQ),AL2(19,3),C'USD'                                       
         DC    AL1(CONQ),AL2(22,20),C'</pidx:CurrencyCode>'                     
         DC    AL1(EORQ),AL2(4+19+3+20)                                         
*                                                                               
         DC    AL1(CONQ),AL2(0,17),C'</pidx:TaxAmount>'                         
         DC    AL1(EORQ),AL2(4+17)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,11),C'</pidx:Tax>'                               
         DC    AL1(EORQ),AL2(4+11)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,23),C'</pidx:InvoiceLineItem>'                   
         DC    AL1(EORQ),AL2(4+23)                                              
*                                                                               
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
*        END OF INVOICE                                                         
*                                                                               
YNCVTOM  DS    0X                                                               
         DC    AL1(CONQ),AL2(0,22),C'</pidx:InvoiceDetails>'                    
         DC    AL1(EORQ),AL2(4+22)                                              
*                                                                               
*        INVOICE SUMMARY                                                        
*                                                                               
         DC    AL1(CONQ),AL2(0,21),C'<pidx:InvoiceSummary>'                     
         DC    AL1(EORQ),AL2(4+21)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,21),C'<pidx:TotalLineItems>'                     
         DC    AL1(LWSQ),AL2(21,L'CHVLITEM,CHVLITEM-LWSD)                       
         DC    AL1(EORQ),AL2(4+21+L'CHVLITEM)                                   
*                                                                               
         DC    AL1(CONQ),AL2(0,19),C'<pidx:InvoiceTotal>'                       
         DC    AL1(EORQ),AL2(4+19)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,21),C'<pidx:MonetaryAmount>'                     
         DC    AL1(LWSQ),AL2(21,L'CHVIAMT,CHVIAMT-LWSD)                         
         DC    AL1(EORQ),AL2(4+21+L'CHVIAMT)                                    
*                                                                               
         DC    AL1(CONQ),AL2(0,19),C'<pidx:CurrencyCode>'                       
         DC    AL1(CONQ),AL2(19,3),C'USD'                                       
         DC    AL1(CONQ),AL2(22,20),C'</pidx:CurrencyCode>'                     
         DC    AL1(EORQ),AL2(4+19+3+20)                                         
*                                                                               
         DC    AL1(CONQ),AL2(0,20),C'</pidx:InvoiceTotal>'                      
         DC    AL1(EORQ),AL2(4+20)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,10),C'<pidx:Tax>'                                
         DC    AL1(EORQ),AL2(4+10)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,18),C'<pidx:TaxTypeCode>'                        
         DC    AL1(CONQ),AL2(18,5),C'Other'                                     
         DC    AL1(CONQ),AL2(18+5,19),C'</pidx:TaxTypeCode>'                    
         DC    AL1(EORQ),AL2(4+18+5+19)                                         
*                                                                               
         DC    AL1(CONQ),AL2(0,16),C'<pidx:TaxAmount>'                          
         DC    AL1(EORQ),AL2(4+16)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,21),C'<pidx:MonetaryAmount>'                     
         DC    AL1(CONQ),AL2(21,4),C'0.00'                                      
         DC    AL1(CONQ),AL2(21+4,22),C'</pidx:MonetaryAmount>'                 
         DC    AL1(EORQ),AL2(4+21+4+22)                                         
*                                                                               
         DC    AL1(CONQ),AL2(0,19),C'<pidx:CurrencyCode>'                       
         DC    AL1(CONQ),AL2(19,3),C'USD'                                       
         DC    AL1(CONQ),AL2(22,20),C'</pidx:CurrencyCode>'                     
         DC    AL1(EORQ),AL2(4+19+3+20)                                         
*                                                                               
         DC    AL1(CONQ),AL2(0,17),C'</pidx:TaxAmount>'                         
         DC    AL1(EORQ),AL2(4+17)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,11),C'</pidx:Tax>'                               
         DC    AL1(EORQ),AL2(4+11)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,22),C'</pidx:InvoiceSummary>'                    
         DC    AL1(EORQ),AL2(4+22)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,15),C'</pidx:Invoice>'                           
         DC    AL1(EORQ),AL2(4+15)                                              
*                                                                               
**                                                                              
**  END OF INVOICE DATA                                                         
**                                                                              
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
YNCVEND  DS    0X       END OF INVOICES                                         
         DC    AL1(CONQ),AL2(0,18),C'</dds:DataObjects>'                        
         DC    AL1(EORQ),AL2(4+18)                                              
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
*        KRAFT TABLES                                                           
*                                                                               
MVKRHDM  DS    0X                                                               
*                                                                               
         DC    AL1(CONQ),AL2(0,39),C'<?xml version="1.0" encoding="UTF-X        
               8" ?>'                                                           
         DC    AL1(EORQ),AL2(4+39)                                              
         DC    AL1(CONQ),AL2(0,15),C'<MediaBillings>'                           
         DC    AL1(EORQ),AL2(4+15)                                              
         DC    AL1(EOTQ,EOTQ)                                                   
**                                                                              
**  END OF HEADER DATA                                                          
*                                                                               
**                                                                              
MVKRIVM  DS    0X                      INVOICE DATA                             
         DC    AL1(CONQ),AL2(0,14),C'<MediaBilling>'                            
         DC    AL1(EORQ),AL2(4+14)                                              
*                                                                               
*        Media here is my suggestion                                            
*        turns out they don't want it                                           
*                                                                               
****     DC    AL1(CONQ),AL2(0,07),C'<Media>'                                   
****     DC    AL1(LWSQ),AL2(07,L'MEDIA,MEDIA-LWSD)                             
****     DC    AL1(CONQ),AL2(07+L'MEDIA,08),C'</Media>'                         
****     DC    AL1(EORQ),AL2(4+07+L'MEDIA+08)                                   
*                                                                               
MVKRIDT  DC    AL1(CONQ),AL2(0,08),C'<Client>'                                  
         DC    AL1(LWSQ),AL2(08,L'KFTCLT,KFTCLT-LWSD)                           
         DC    AL1(CONQ),AL2(08+L'KFTCLT,09),C'</Client>'                       
         DC    AL1(EORQ),AL2(4+08+L'KFTCLT+09)                                  
         DC    AL1(CONQ),AL2(0,09),C'<Product>'                                 
         DC    AL1(LWSQ),AL2(09,L'KFTPRD,KFTPRD-LWSD)                           
         DC    AL1(CONQ),AL2(09+L'KFTPRD,10),C'</Product>'                      
         DC    AL1(EORQ),AL2(4+09+L'KFTPRD+10)                                  
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
MVKRIDT2 DC    AL1(CONQ),AL2(0,21),C'<Estimate></Estimate>'                     
         DC    AL1(EORQ),AL2(4+21)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,19),C'<EstName></EstName>'                       
         DC    AL1(EORQ),AL2(4+19)                                              
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
MVKRIDT3 DC    AL1(CONQ),AL2(0,10),C'<Estimate>'                                
         DC    AL1(LWSQ),AL2(10,L'KFTEST,KFTEST-LWSD)                           
         DC    AL1(CONQ),AL2(10+L'KFTEST,11),C'</Estimate>'                     
         DC    AL1(EORQ),AL2(4+10+L'KFTEST+11)                                  
*                                                                               
         DC    AL1(CONQ),AL2(0,09),C'<EstName>'                                 
         DC    AL1(LWSQ),AL2(09,L'KFTESTN,KFTESTN-LWSD)                         
         DC    AL1(CONQ),AL2(09+L'KFTESTN,10),C'</EstName>'                     
         DC    AL1(EORQ),AL2(4+09+L'KFTESTN+10)                                 
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
MVKRIDT4 DC    AL1(CONQ),AL2(0,09),C'<Invoice>'                                 
         DC    AL1(LWSQ),AL2(09,L'KFTINV,KFTINV-LWSD)                           
         DC    AL1(CONQ),AL2(09+L'KFTINV,10),C'</Invoice>'                      
         DC    AL1(EORQ),AL2(4+09+L'KFTINV+10)                                  
         DC    AL1(CONQ),AL2(0,09),C'<InvDate>'                                 
         DC    AL1(LWSQ),AL2(09,L'KFTIDTE,KFTIDTE-LWSD)                         
         DC    AL1(CONQ),AL2(09+L'KFTIDTE,10),C'</InvDate>'                     
         DC    AL1(EORQ),AL2(4+09+L'KFTIDTE+10)                                 
*                                                                               
         DC    AL1(CONQ),AL2(0,11),C'<InvAmount>'                               
         DC    AL1(LWSQ),AL2(11,L'MVKDDUE,MVKDDUE-LWSD)                         
         DC    AL1(EORQ),AL2(4+11+L'MVKDDUE)                                    
*                                                                               
         DC    AL1(CONQ),AL2(0,09),C'<TAX-HST>'                                 
         DC    AL1(LWSQ),AL2(09,L'MVKTAX,MVKTAX-LWSD)                           
         DC    AL1(EORQ),AL2(4+09+L'MVKTAX)                                     
*                                                                               
         DC    AL1(CONQ),AL2(0,11),C'<AmountDue>'                               
         DC    AL1(LWSQ),AL2(11,L'MVKTDUE,MVKTDUE-LWSD)                         
         DC    AL1(EORQ),AL2(4+11+L'MVKTDUE)                                    
*                                                                               
*        SEND AN EMPTY ReqNumber FIELD                                          
*        REQUESTED BY HEATHER GIBSON APR17/2012                                 
*                                                                               
         DC    AL1(CONQ),AL2(0,23),C'<ReqNumber></ReqNumber>'                   
         DC    AL1(EORQ),AL2(4+23)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,12),C'<eBCANumber>'                              
         DC    AL1(LWSQ),AL2(12,L'KFTEBC,KFTEBC-LWSD)                           
         DC    AL1(EORQ),AL2(4+12+L'KFTEBC)                                     
                                                                                
         DC    AL1(CONQ),AL2(0,10),C'<Currency>'                                
         DC    AL1(LWSQ),AL2(10,L'KFTCUR,KFTCUR-LWSD)                           
         DC    AL1(CONQ),AL2(10+L'KFTCUR,11),C'</Currency>'                     
         DC    AL1(EORQ),AL2(4+10+L'KFTCUR+11)                                  
*                                                                               
         DC    AL1(CONQ),AL2(0,15),C'</MediaBilling>'                           
         DC    AL1(EORQ),AL2(4+15)                                              
**                                                                              
**  END OF INVOICE DATA                                                         
**                                                                              
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
MVKREND  DS    0X                                                               
         DC    AL1(CONQ),AL2(0,16),C'</MediaBillings>'                          
         DC    AL1(EORQ),AL2(4+16)                                              
*                                                                               
         DC    AL1(EOTQ,EOTQ)                                                   
         EJECT                                                                  
*                                                                               
DATAFILE DCB   DDNAME=DATAFILE,DSORG=PS,MACRF=GM,                      X        
               RECFM=FB,LRECL=324,EODAD=DATAEND                                 
*                                                                               
*              LRECL SHOULD BE 324 - 309 TO TRY AND READ DONNA'S                
*              KRAFT TEST FILE + BLKSIZE=3090                                   
*                                                                               
XMLMAP   DCB   DDNAME=XMLMAP,                                          X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=400,                                              X        
               BLKSIZE=4000,                                           X        
               MACRF=PM                                                         
*                                                                               
         DC    C'*OUTREC*'                                                      
         DS    F                   FOR RECORD LENGTH                            
OUTREC   DS    400C                                                             
*                                                                               
*                                                                               
         DC    C'**REC***'                                                      
REC      DS    400C                                                             
*                                                                               
         EJECT                                                                  
         DS    0D                                                               
         DC    C'**I/O***'                                                      
IO       DS    3000C                                                            
         EJECT                                                                  
*********************************************************************           
* DSECT TO COVER MAP TABLE ENTRY                                    *           
*********************************************************************           
                                                                                
MAPD     DSECT                                                                  
MAPTYP   DS    AL1                 TYPE OF ENTRY TO FOLLOW                      
ROUQ     EQU   1                   ROUTINE                                      
LWSQ     EQU   2                   FROM LOCAL SAVED DATA                        
CONQ     EQU   3                   CONSTANT                                     
BEGQ     EQU   4                   BEGIN LOOP                                   
ENDQ     EQU   5                   END LOOP                                     
MAPROUT  DS    XL2                 DISPLACEMENT TO ROUTINE                      
MAPRLNQ  EQU   *-MAPD                                                           
         ORG   MAPROUT                                                          
MAPFLD   DS    AL2                 DISPLACEMENT TO OUTPUT FIELD                 
MAPLEN   DS    AL2                 LENGTH OF OUTPUT FIELD (OR ZERO)             
MAPDATA  DS    XL2                 DISPLACEMENT TO DATA                         
MAPLNQ   EQU   *-MAPD                                                           
         ORG   MAPDATA                                                          
MAPCON   DS    0C                  CONSTANT                                     
         ORG                                                                    
*                                                                               
EORQ     EQU   X'00'               END OF RECORD                                
EOTQ     EQU   X'FF'               END OF TABLE                                 
ALL      EQU   X'FF'                                                            
EOT      EQU   X'FF'                                                            
*                                                                               
         EJECT                                                                  
LWSD     DSECT                   WORKING STORAGE                                
PARMS    DS    0F                                                               
AEDINME  DS    AL4                 IF FIRST BYTE IS C'T' = TEST                 
*                                  DATASET NAME WILL START WITH                 
*                                  SPTFDISK.TEST (INSTEAD OF .PROD)             
COMFAC   DS    AL4                                                              
ADMASTC  DS    AL4                                                              
APLINE   DS    AL4                                                              
AREPORT  DS    AL4                                                              
PARMSLNQ EQU   *-PARMS                                                          
*                                                                               
SAVR1    DS    F                                                                
FCS      DS     X'00'              FILE CONTROL SWITCH                          
FCSIN    EQU    X'80'              DATAFILE (INPUT) FILE IS OPEN                
FCSEDI   EQU    X'40'              EDI  (OUTPUT) FILE IS OPEN                   
FCSPROC  EQU    X'20'              IN PROCESS  ROUTINE                          
FCSSORT  EQU    X'10'              IN SORT ROUTINE                              
FCSEIO   EQU    X'08'              EI OUTPUT USED                               
FCSSORE  EQU    X'01'              SORT END - COMPLETED                         
LASTINV  DS     CL10                                                            
*                                                                               
COUNTER  DS    F                   RECORD COUNTER                               
SVDSKADD DS    F                   SAVED DISK ADDRESS                           
ERRCNT   DS    F                   ERROR RECORD COUNTER                         
VREMOTEC DS    F                                                                
*                                                                               
DATADISP DS    H                   DISPLACEMENT TO FIRST ELEMENT                
*                                                                               
INVDOLS  DS    PL8                 FOR TOTALING INVOICE $                       
*                                                                               
RUNDOLS  DS    PL8                 RUN $ TOTAL                                  
*                                  FOR KRAFT - INCLUDES TAXES                   
*                                                                               
RUNINVS  DS    PL8                 RUN INVOICES                                 
RUNSUPL  DS    CL15                SUPPLIER ID                                  
*                                                                               
RUNCTAX  DS    PL8                 CANADIAN TAXES (KRAFT)                       
RUNINVD  DS    PL8                 INVOICE AMT (KRAFT)                          
*                                                                               
TEMPDATE DS    XL3                 TEMPORARY AREA FOR PACKED DATE               
RECCHG   DS    CL1                                                              
ERRSW    DS    CL1                                                              
*                                                                               
PQIDNUM  DS    XL2                 USER ID NUM FOR PQ DEST                      
FIRSTINV DS    CL1                                                              
OPENPQ   DS    XL1                                                              
LASTALPH DS    CL2                                                              
*                                                                               
ELEMFND  DS    XL1                 SWITCH - RECONCILED ELEMENT MARKED           
*                                                                               
THISSE   DS    XL1                      SE NUMBER                               
*                                                                               
YNASW    DS    CL1                 SET TO Y IF DOING YNR -ARIBIA                
KFTSW    DS    CL1                 SET TO Y IF DOING KRAFT FORMAT               
KFTTYP   DS    CL1                 G= GROCERY - CLIENT FGM                      
*                                  N= SNACK - CLIENT MDM                        
*                                  SET FROM FIRST INPUT RECORD                  
CARSW    DS    CL1                 SET TO Y IF DOING CARAT FORMAT               
*                                                                               
CARD     DS    CL80                                                             
*                                                                               
CONROUT  DS    A                   A(CONTROL ROUTINE)                           
RELO     DS    A                                                                
BEFRQ    EQU   240                 BEFORE READING INPUT CARDS                   
AFTRQ    EQU   241                 AFTER READING INPUT CARDS                    
*                                                                               
VCASHVAL DS    V                                                                
AMQRPT   DS    V                   A(MQRPT)                                     
ASORTAB  DS    A                   A(INVOICE SORT TABLE)                        
VSORTER  DS    V                   A(SORTER)                                    
VTICTOC  DS    V                   A(TICTOC)                                    
VTIMEOUT DS    V                   A(TIMEOUT)                                   
*                                                                               
MQMAPNM  DS    CL14                SFTPDISK.PROD.                               
*                                                                               
DSNAME   DS    CL35  DSN -  BIL.ACC.AGID.DYYYMMDD.THHMMSS                       
*                                                                               
MAPNME   DS    0CL20               (PRTTAPE.PP0AEXX)                            
MAPTAPE  DS    CL7                 PRTTAPE                                      
         DS    CL1                 .                                            
MAPSYS   DS    CL3                 PP0                                          
MAPPGM   DS    CL2                 AE                                           
MAPAGY   DS    CL2                 XX                                           
MAPSUFX  DS    CL1                 SUFFIX                                       
         DS    CL4                 N/D                                          
*                                                                               
SYSTM    DS    CL1                 SYSTEM                                       
CLI      DS    CL3                 CLIENT                                       
MEDDESC  DS    CL10                MEDIA DESCRIPTION                            
*                                                                               
CTODAY   DS    CL8                                                              
         ORG   CTODAY                                                           
TYYYY    DS    CL4                 TODAY'S YEAR,MONTH,DAY                       
TMM      DS    CL2                                                              
TDAY     DS    CL2                                                              
*                                                                               
TIMEOFD  DS    CL8                 TIME OF DAY HH.MM.SS                         
*                                                                               
MYFULL   DS    F                                                                
SAVER1   DS    F                                                                
AAGC     DS    A                   A(AGENCY CLIENT TABLE)                       
AAGYSTAB DS    A                   A(AGENCY SYSTEM TABLE)                       
*                                                                               
INVNUMB  DS    CL(L'CINVNUM)                                                    
*                                                                               
BKLQ     EQU   8                                                                
BKS      DS    0D                                                               
BASAMT   DS    PL(BKLQ)            BASE AMOUNT                                  
COMAMT   DS    PL(BKLQ)            COMMISSION                                   
COMAMT1  DS    PL(BKLQ)            COMMISSION FROM AS IN ADCOMAMT               
DUEAMT   DS    PL(BKLQ)            DUE                                          
TAXAMT   DS    PL(BKLQ)            TAX                                          
*                                                                               
TOTTAX   DS    PL(BKLQ)                                                         
TOTNET   DS    PL(BKLQ)                                                         
TOTCSD   DS    PL(BKLQ)                                                         
*                                                                               
CALCDUE  DS    PL(BKLQ)            CALCULATED AMOUNT DUE                        
BIGCOM   DS    PL(BKLQ)            BIGGEST COMMISSION                           
*                                                                               
MITOTGRS DS    PL(BKLQ)            MEDIA INVOICE TOTAL GROSS                    
MITOTNET DS    PL(BKLQ)            MEDIA INVOICE TOTAL NET                      
*                                                                               
GSTTAX   DS    PL(BKLQ)            GST TAX AMOUNT PACKED                        
HSTTAX   DS    PL(BKLQ)            HST TAX AMOUNT PACKED                        
PSTTAX   DS    PL(BKLQ)            PST TAX AMOUNT PACKED                        
QSTTAX   DS    PL(BKLQ)            QST TAX AMOUNT PACKED                        
*                                                                               
GSTTAXT  DS    PL(BKLQ)            GST TAX AMOUNT PACKED                        
HSTTAXT  DS    PL(BKLQ)            HST TAX AMOUNT PACKED                        
PSTTAXT  DS    PL(BKLQ)            PST TAX AMOUNT PACKED                        
QSTTAXT  DS    PL(BKLQ)            QST TAX AMOUNT PACKED                        
*                                                                               
TOTDUE   DS    PL(BKLQ)            TOTAL DUE AMOUNT PACKED                      
NBKS     EQU   (*-BKS)/BKLQ                                                     
*                                                                               
*                                                                               
ZEROS    DS    CL20                                                             
PZERO    DS    PL1                                                              
TESTMQ   DS    CL1                                                              
SVQOPT2  DS    CL1        SAVED FROM QOPT2 AT REQFRST                           
SVQOPT5  DS    CL1        SAVED FROM QOPT5 AT REQFRST                           
ELEM     DS    CL200                                                            
*                                                                               
SAVRE    DS    F                                                                
PL16     DS    PL16                                                             
PL8      DS    PL8                                                              
SVRE     DS    F                                                                
FILESW   DS    CL1                                                              
RTNROU   DS    C                   RETURN TO ROUTINE 'Y' OR 'N'                 
PRTNROU  DS    C                   RETURN TO ROUTINE 'Y' OR 'N'                 
RTNADR   DS    F                   A(START OF MAP DATA)                         
PREVADR  DS    F                   A(START OF MAP DATA)                         
*                                                                               
HDRSENT  DS    CL1                 Y=HEADER HAS BEEN SENT                       
*                                                                               
SKIPSET  DS    CL1                                                              
SKIPINV  DS    CL1                                                              
SKIPEIN  DS    XL1                                                              
SKIPCNT  DS    XL2                                                              
SKIPAMT  DS    PL8                                                              
PIDCNT   DS    PL8                 TEMPORARY COUNTER                            
INSCNT   DS    PL8                 TEMPORARY INSERTIONS COUNTER                 
MYDUB    DS    PL8                                                              
*                                                                               
ANYTAX   DS    CL1                                                              
*                                                                               
AIDPITM  DS    A                   A(CURRENT ID RECORD DETAIL)                  
IDPCNT   DS    F                   NUMBER IN LINE ITEM TABLE                    
ALINITM  DS    A                   A(CURRENT LINE ITEM DETAIL)                  
LINCNT   DS    F                   NUMBER IN LINE ITEM TABLE                    
AMITITM  DS    A                   A(CURRENT MONTHLY INVOICE TOTAL)             
MITCNT   DS    F                   NUMBER IN MONTHY INVOICE TABLE               
*                                                                               
AGYALPHA DS    CL2                 AGENCY APHHA                                 
AGYNAME  DS    CL40                AGENCY NAME                                  
AGYNAM33 DS    CL33                AGENCY NAME - FROM BILLING                   
PAYSITE  DS    CL14                PAY SITE                                     
POLN     DS    CL10                PURCHASE ORDER LINE NUMBER                   
DUEWDEC  DS    CL11                AMOUNT DUE WITH A DECIMAL                    
RLSN     DS    CL2                 RELEASE NUMBER                               
*                                                                               
AGYID    DS    CL5                                                              
AGYPROF  DS    CL15                                                             
PO#      DS    CL10                                                             
*                                                                               
WBFLT    DS    CL10                                                             
BILLCM   DS    CL80                                                             
*                                                                               
BASIS    DS    CL1                                                              
*                                                                               
ABIGLINE DS    A                   A(LINE WITH BIGGEST COMMISSION)              
*                                                                               
ITMCNT   DS    PL4                 ITEM COUNT                                   
CITMCNT  DS    CL6                                                              
*                                                                               
INVDCYMD DS    CL8                 INVOICE DATE CCYYMMDD                        
         ORG   INVDCYMD                                                         
CCYY     DS    CL4                 CCYY                                         
MM       DS    CL2                 MM  (MONTH)                                  
DD       DS    CL2                 DD  (DAY)                                    
DUEDCYMD DS    CL8                 INVOICE DUE DATE CCYYMMDD                    
DUEDMDY  DS    CL10                INVOICE DUE DATE MM/DD/YYYY                  
*                                                                               
FISCAL   DS    CL4                 FISCAL -  CCYY                               
FISCALA  DS    CL1                 APPEND AN 'A' FOR SECOND HALF                
*                                                                               
ITMDCYMD DS    0CL8                FIRST LINE ITEM DATE CCYYMMDD                
ITMDCY   DS    CL4                 FIRST LINE ITEM DATE CCYY                    
ITMDMON  DS    CL2                 FIRST LINE ITEM DATE MM                      
ITMDDAY  DS    CL2                 FIRST LINE ITEM DATE DD                      
ITMMCY   DS    CL6                 MMCCYY - FROM ABOVE                          
SVPO#    DS    CL(L'PO#)           SAVED PO# (BEFORE DASHES REMOVED)            
*                                                                               
*   CHEVRON DATA                                                                
*                                                                               
         DS    CL1      NOT SPARE                                               
CHVLDUE  DS    CL35                FROM CLINDUE                                 
CHVDNET  DS    CL35                FROM CDUENET                                 
CHVDCOM  DS    CL35                FROM CDUECOM                                 
         DS    CL1      NOT SPARE                                               
CHVPO#   DS    CL60                                                             
         DS    CL1      NOT SPARE                                               
CHVURL   DS    CL76                                                             
*                                                                               
CHVPLCD  DS    CL4                 FROM CPLANTC                                 
CHVINVD  DS    CL10                FROM CBILLDTE                                
CHVSPLID DS    CL10                FROM CSUPPLR                                 
*                                                                               
*   KRAFT DATA                                                                  
*                                                                               
KFTCLT   DS    CL3     FROM KCLT                                                
KFTPRD   DS    CL3     FROM KPRD                                                
KFTEST   DS    CL8     FROM KEST                                                
KFTESTN  DS    CL8     FROM KESTNAME                                            
KFTINV   DS    CL10    FROM KINV                                                
KFTIDTE  DS    CL10    FROM KINVDTE                                             
         DS    CL1      NOT SPARE                                               
MVKDDUE  DS    CL26                KAMT WITH </InvAmount>                       
         DS    CL1      NOT SPARE                                               
MVKTAX   DS    CL26                KTAX WITH </TAX-HST>                         
         DS    CL1      NOT SPARE                                               
MVKTDUE  DS    CL26                KDUE WITH </AmountDue>                       
         DS    CL1      NOT SPARE                                               
KFTEBC   DS    CL35                FROM KEBCAN WITH </eBCANumber>               
KFTCUR   DS    CL3     FROM KCUR                                                
         EJECT                                                                  
AGYDATA  DS    0X                  SPECIAL AGENCY DATA                          
*                                                                               
         ORG   AGYDATA                                                          
*        CHEVRON XML SPECIAL FIELDS                                             
*                                                                               
CHVWORKC DS    CL50     WORKCODE - FROM FISRT 2 BYTES OF CWKTSK                 
*                       OR HARDCODED                                            
*                       INCLUDES </pidx...                                      
*                                                                               
         DS    CL1      NOT SPARE                                               
CHVIAMT  DS    CL50              FROM INVDOLS WITH </pidx:Monetary...           
*                                                                               
         DS    CL1      NOT SPARE                                               
CHVFTR   DS    CL200             FIELD TICKET NUMBER                            
*                   FROM CACCBOTH +</pidx...                                    
         DS    CL1      NOT SPARE                                               
CHVDESC  DS    CL200             LINE ITEM DESCRIPTION                          
*                   FROM CCONTRA/CWKTSK/CREF +</pidx...                         
         DS    CL1      NOT SPARE                                               
CHVPUB   DS    CL100                                                            
*                                                                               
         DS    CL1      NOT SPARE                                               
CHVSUPL  DS    CL60                                                             
*                                                                               
         DS    CL1      NOT SPARE                                               
CHVSEA   DS    CL100    FROM CUSER1 +</pidx...                                  
*                                                                               
         DS    CL1      NOT SPARE                                               
CHVITEM  DS    CL50     LINE ITEM NUMBER                                        
*                                                                               
         DS    CL1      NOT SPARE                                               
CHVLITEM DS    CL50     LINE ITEM TOTAL                                         
*                                                                               
         DS    CL200               SPARE JUST IN CASE                           
         ORG                                                                    
         EJECT                                                                  
*                                                                               
LWSX     EQU   *                                                                
*                                                                               
LINED    DSECT                                                                  
         DS    CL4                                                              
LAGY     DS    CL2                 ALPHA AGENCY                                 
         DS    CL10                                                             
LMED     DS    CL1                 MEDIA                                        
         DS    CL6                                                              
LCLT     DS    CL3                 CLIENT                                       
         DS    CL6                                                              
LPUB     DS    CL11                PUB                                          
         DS    CL3                                                              
LDATE    DS    CL8                 DATE                                         
         DS    CL5                                                              
LCKNUM   DS    CL6                 CHECK NUMBER                                 
         DS    CL9                                                              
LSEQNUM  DS    CL3                 SEQUENCE NUMBER                              
         DS    CL2                                                              
LSTATUS  DS    CL1                 STATUS                                       
         DS    CL2                                                              
LBNKDT   DS    CL8                 BANK CLEARED DATE                            
         DS    CL2                                                              
LINVOICE DS    CL12                INVOICE NUMBER                               
         SPACE 3                                                                
*                                                                               
* RECORD LAYOUT FOR CHEVRON - ALL AGENCIES                                      
*                                                                               
CRECD    DSECT                                                                  
CPLANTC  DS    CL4                 PLANT CODE                                   
CACCBOTH DS    CL99                SJ LEDGER CLIENT/PRODUCT/JOB + NAME          
         DS    CL1                 NOT USED                                     
CBILLDTE DS    CL10                BILL RUN DATE                                
CINVNUM  DS    CL09                INVOICE NUMBER                               
CCOUNT   DS    CL4                 LINE ITEM NUM - RESET FOR EACH INV.          
CUSER1   DS    CL30                USER - SERVICE ENTRY APPROVER                
CUSER2   DS    CL30                USER - SERVICE ORDER NO.                     
CSUPPLR  DS    CL10                ACCOUNT EQUIV - SUPPLIER ID CODE             
CNETBILL DS    CL13                BILLED NET DOLLARS                           
CWKTSK   DS    CL36                WORKCODE/TASKCODE NAME                       
CCONTRA  DS    CL36                CONTRA-ACCOUNT CODE + NAME                   
CREF     DS    CL6                                                              
CNARR    DS    CL36                                                             
CRECLNQ  EQU   *-CRECD             LENGTH OF RECORD                             
*                                                                               
         EJECT                                                                  
*                                                                               
* RECORD LAYOUT FOR KRAFT - ALL AGENCIES                                        
*                                                                               
KRECD    DSECT                                                                  
KCLT     DS    CL3                 CLIENT CODE                                  
KPRD     DS    CL3                 PRODUCT CODE                                 
KEST     DS    CL8                 GAP                                          
KESTNAME DS    CL8                 GAP                                          
KINV     DS    CL10                INVOICE NUMBER                               
KINVDTE  DS    CL10                INVOICE DATE MM/DD/YYYY                      
KINVAMT  DS    CL13                INVOICE AMOUNT                               
KTAX     DS    CL13                TAX-HST                                      
KAMTDUE  DS    CL13                TOTAL DUE                                    
KREQNUM  DS    CL10                GAP                                          
KEBCAN   DS    CL16                EBCANMUMBER                                  
KCUR     DS    CL12                CURRENCY                                     
         DS    CL205               SPARE TO PAD TO 324                          
*                                                                               
KRECLNQ  EQU   *-KRECD             LENGTH OF RECORD                             
*                                                                               
         EJECT                                                                  
*                                                                               
MQMSGD   DSECT                                                                  
MQHID    DS    CL6                 HUB RECORD ID                                
MQSYS    DS    CL3                 SYSTEM                                       
MQAGYID  DS    CL4                 AGENCY 1D 4-CHAR                             
MQQUAL   DS    CL16                QUALIFIER                                    
MQDATE   DS    CL6                 YYMMDD OF DSN                                
MQTIME   DS    CL6                 HHMMSS OF DSN                                
MQDATA1  DS    CL32                NOT USED                                     
MQDATA2  DS    CL32                NOT USED                                     
MQFILE   DS    CL64                DSN  (MINUS SFTPDISK.PROD.)                  
MQMSGLNQ EQU   *-MQMSGD                                                         
*                                                                               
*PPMODEQU                                                                       
*PPREPWORK                                                                      
*PPREPWORK2                                                                     
*PPNEWFILE                                                                      
*CTGENFILE                                                                      
*DDMASTD                                                                        
*DDREMOTED                                                                      
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027PPREPAE02 03/08/16'                                      
         END                                                                    
