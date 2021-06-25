*          DATA SET PPREP4802  AT LEVEL 084 AS OF 04/19/16                      
*PROCESS USING(WARN(15))                                                        
*PHASE PP4802A                                                                  
*INCLUDE OUTER                                                                  
*INCLUDE PPGETADR                                                               
*INCLUDE PPECHOP                                                                
*INCLUDE PRNTOFC                                                                
         TITLE 'CHANGE LOG  '                                                   
*                                                                               
* BPLA 10/13     FILTER ON LOCKED PUBS                                          
*                                                                               
* BPLA 12/11     PUBAC-DISPLAY 100.000 WHEN =P'-1'                              
*                WAS DISPLAYED AS 100.00                                        
*                                                                               
* SMYE 11/05     2-CHR MEDIA OFFICE CHANGES                                     
*                                                                               
* KWAN 04/00     CHANGES FOR PPNEWFILE (LARGER PCONREC, ETC.)                   
*                                                                               
* SMYE 03/03/00  MODIFY PREMIUMS FOR CLIENT-SPECIFIC RECORDS                    
*                                                                               
* BPLA 12/99     FIX USE OF PPGETADR                                            
*                                                                               
* SMYE 11/99     ADD EXCLUSIONS TO PROD LISTINGS (TYPE "P")                     
*                                                                               
* KWAN 10/99     ADD STATE CODE FILTER IN SCFILTER (RCARD COL 60-61)            
*                                                                               
* KWAN 07/99     REMOVE ELEM LEN CHKING FOR PUB ADDR RECS                       
*                                                                               
* BPLA  04/99    DISPLAY PUB FREQ (FOR MAGAZINES)                               
*                                                                               
* SMYE 03/99     ADD EMAIL TO ADDRESS LISTINGS (QOPT1 1,2,3,4)                  
*                                                                               
* SMYE 11/98     NEW ONE-UP LISTING - WEB SITE - LIST TYPE I                    
*                                                                               
* BPLA 4/98      NEW OPTION TO USE PPGETADDR FOR ONE CLT OR OFFICE              
*                ADDRESS LISTINGS (QOPT1 1,2,3,4)                               
*                CONTROLLED BY P48 PROFILE                                      
*                                                                               
* BPLA 4/98      NEW OPTION FOR INCLUDING UNASSIGNED PUBS                       
*                FOR LIST TYPES V AND G (QOPT 6)                                
*                                                                               
* BPLA 9/97      FIX HEADLINES IF PUBLISHER LISTING                             
*                USE HEAD6 NOT HEAD5                                            
*                ALSO IF FILTERING ON LANGUAGE                                  
*                NOTE THAT IN HEADLINES                                         
*                AND SHORTEN HEADLINE MESSAGES                                  
*                                                                               
* SMYE 7/97      CHANGES TO READ CONTROL FILE FAX RECORDS                       
*                    (PBUYREC USED FOR I/O AREA IN GETFAX CSECT)                
*                                                                               
* SMYE 5/97      USE EITHER PUB ADDRESS RECORDS OR ADDRESS ELEMENTS             
*                AT ADDR..                                                      
*                                                                               
* SMYE 1/97      FIX PUB COUNTS                                                 
*                                                                               
* SMYE 11/96     ADDED PUB COUNT TO LISTINGS                                    
*                                                                               
* SMYE 6/96      ADDED LISTING - PUB GROUP ASSIGNMENTS                          
*                     LIST TYPE = G (PGA)                                       
*                                                                               
* BPLA 6/96      IF LIST TPYE = K (RCODES) OR S (SAURS)                         
*                SAVE AND RESTORE REQUESTED MEDIA                               
*                WAS BEING RESET TO "N"                                         
*                                                                               
* SMYE 04/96     PUBLISHER PUBLIST (QPUB+1 = P)                                 
*                                                                               
* SMYE 12/13/95  CHANGED DTCNV TO DATCON WITH NEW PARAM'S                       
*                                                                               
* SMYE 12/95     ADDED ONE-UP LISTING - "PAY" OPTION - LIST TYPE Y              
*                                                                               
* BPLA 11/95     DISPLAY A "F" BEFORE PUB NUMBERS ON THREE-UP LISTINGS          
*                                                                               
* BPLA 10/95     DISPLAY LANGUAGE - ON ONE-UP LISTING                           
*                ALSO LANGUAGE FILTER (QOPT 5)                                  
*                                                                               
* BPLA 11/93     ADD DISPLAY OF PST CODES                                       
*                                                                               
* BPLA 7/2/92    ADD KILL DATE FILTER (SKIP PUBS WHOSE KILL DATE                
*                IS ON OR BEFORE THIS DATE) IN QPAY (COL 53)                    
* BPLA 1/30/92   DON'T REQUIRE X'14' ELEM FOR QOPTS 1-4 (ADDRS)                 
*                IF CLIENT GIVEN                                                
* BPLA 11/1/91   CHANGE TO ADV-AOR LOGIC FOR 3 CHARACTER ADV'S                  
*                ALSO - DON'T TRY TO READ QCLIENT  - IT IS THE ADV              
*                HEADLINES FOR ADV-AOR LISTING (QOPT1 = V)                      
*                                                                               
* BPLA 10/9/91   ADD QOPT1 = V = ADV-AOR ASSIGNMENTS                            
*                                                                               
* BPLA 3/28/91   FOR QOPT4 = C LOOK FOR MASTER CONTRACTS                        
*                              IF SLAVE CLIENT                                  
* BPLA 3/26/91   ADD QOPT4 - ACTIVE PUBS ONLY OPTION (PUBS WITH BUYS)           
*                            Y = BUYS OR CONTRACTS                              
*                            B = BUYS                                           
*                            C = CONTRACTS                                      
*                                                                               
* ROSA 11/15/90  PRING GST TAX WITH RATES                         L02           
* ROS  10/1/90 'FCGTPUB' NOT BEING RESET PROPERLY                 BUG01         
*                                                                               
* SWON 12/26/89 CHECK FOR FAX FIELD, PRINT FAX NUMBER                           
*                                                                               
         TITLE 'PUBLICATION LISTINGS'                                           
         SPACE  3                                                               
******************************************                                      
*        QOPT1  LIST TYPE                                                       
*                                                                               
*               THREE-UP LISTINGS                                               
*                                                                               
*               N (OR BLANK) = NAME                                             
*               Z = NAME AND ZONE                                               
*               A = NAME,ZONE AND ADDRESS                                       
*                                                                               
*               ONE-UP LISTINGS                                                 
*                                                                               
*               P = PRODUCTION                                                  
*               C = CIRCULATION                                                 
*               L = CLE (CONTRACT LINAGE EQUIVALENCES                           
*               S = SAU RATES                                                   
*               D = DIVISION/REGION/DISTRICT SCHEMES                            
*               $ = RATES                                                       
*               R = REPS (WITH CLIENT VENDOR NUMBERS)                           
*               1 = PAYING ADDRESSES                                            
*               2 = TRAFFIC ADDRESSES                                           
*               3 = CONTRACT ADDRESSES                                          
*               4 = SHIPPING ADDRESSES                                          
*               B = BUYERS WORKSHEET                                            
*               W = WORKSHEET                                                   
*               K = RCODES (RATE CODES)                                         
*               M = PUB STANDARD COMMENTS (FROM PUBREPELS)                      
*               X = PREMIUMS                                                    
*               V = AOR-ADV PUB LINKS                                           
*               Y = PAY OPTIONS (FROM PUBREPELS)                                
*               G = PUB GROUP ASSIGNMENTS                                       
*               I = WEB SITES                                                   
*                                                                               
*        QOPT2  Y = AGENCY PUBS ONLY (NO ZZ DEFAULTS)                           
*                                                                               
*        QOPT3  Y = DOUBLE SPACING                                              
*                                                                               
*        QOPT4  Y = PUBS WITH BUYS OR CONTRACTS FOR REQUESTED CLIENT            
*               B = PUBS WITH BUYS                                              
*               C = PUBS WITH CONTRACTS                                         
*               L = LOCKED PUBS ONLY                                            
*               U = UNLOCKED PUBS ONLY                                          
*                                                                               
*        QOPT5    = LANGUAGE FILTER                                             
*                   IF NON-SPACE PUBLANG MUST MATCH                             
*                   (E-ENGLISH ALSO MATCHES TO SPACE)                           
*                                                                               
*        QOPT6   Y= INCLUDE UNASSIGNED FOR LIST TYPES V AND G                   
*                N (OR BLANK) = NO UNASSGINED                                   
*                                                                               
*        QOPT7    = SCHEME FILTER                                               
*                   IF NON-SPACE GRPPID (GROUP ID) MUST MATCH                   
*                   (FOR LIST TYPE G)                                           
*                                                                               
         SPACE  3                                                               
PP4802   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PP4802,RR=R9                                                 
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,2048(RC)                                                      
         LA    R9,2048(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R7,SPACEND                                                       
         USING PP48WRKD,R7                                                      
*                                                                               
*                                                                               
*                                                                               
         XC    SCFILTER,SCFILTER   CLEAR STATE CODE FILTER                      
         MVI   RC2DSECT,C'Y'       2ND DSECT                                    
         L     R6,PPWORK2C                                                      
         USING PPWORK2D,R6                                                      
         CLC   QOPT8-2(2),SPACES   ANYTHING IN STATE CODE?                      
         BNH   *+10                                                             
         MVC   SCFILTER,QOPT8-2    STATE CODE FILTER (RCARD2 COL 60-61)         
         MVI   RC2DSECT,C'N'       DONE WITH 2ND DSECT                          
         DROP  R6                                                               
*                                                                               
*                                                                               
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   PUBL0                                                            
*                                                                               
         MVC   DUB,SPACES          GET PSTVAL                                   
         MVC   DUB(6),=C'T00A6B'                                                
         GOTO1 LOADER,DMCB,DUB,0                                                
*                                                                               
         OC    DMCB+4(4),DMCB+4                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   APSTVAL,4(R1)          SAVE ITS ADDR                             
*                                                                               
         L     R8,=A(GETFAX)                                                    
         A     R8,RELO                                                          
         ST    R8,AGETFAX          SAVE ADDRESS OF GETFAX                       
*                                                                               
         L     R8,=V(PRNTOFC)                                                   
         A     R8,RELO                                                          
         ST    R8,VPRNTOFC         SAVE ADDRESS OF PRNTOFC                      
*                                                                               
         XC    DMCB(4),DMCB        NEED ADDRESS OF OFFICER                      
         MVC   DMCB+4(4),=X'D9000A38'                                           
         L     R8,VCOMFACS                                                      
         L     R8,(CCALLOV-COMFACSD)(R8)                                        
         GOTOR (R8),DMCB                                                        
         MVC   VOFFICER,DMCB       SAVE ADDRESS OF OFFICER                      
*                                                                               
         SPACE 2                                                                
         B     PLEXT                                                            
PUBL0    DS    0H                                                               
*                                                                               
         CLI   MODE,FPUBREQ                                                     
         BNE   PUBL1                                                            
         ZAP   PUBCNT,=P'0'                                                     
         MVI   FRSW,0                                                           
         MVI   ENDSW,0             CLEAR FINAL PAGE SWITCH                      
*                                                                               
         MVI   FCGTPUB,C'Y'        RESET TO READ PUB NAME         BUG01         
         XC    KILLDATE,KILLDATE   CLEAR KILLDATE FILTER                        
         XC    KILLDATP,KILLDATP   CLEAR KILLDATE FILTER                        
         CLC   QPAY(6),SPACES                                                   
         BE    PUBL00                                                           
         CLC   QPAY+4(2),=C'00'    SEE IF I HAVE A DAY                          
         BH    *+10                                                             
         MVC   QPAY+4(2),=C'01'    DEFAULT TO FIRST DAY OF MONTH                
*        GOTO1 DTCNV,DMCB,(0,QPAY),(1,KILLDATE)                                 
         GOTO1 DATCON,DMCB,(0,QPAY),(3,KILLDATE)                                
*        GOTO1 DTCNV,DMCB,(0,QPAY),(3,KILLDATP)                                 
         GOTO1 DATCON,DMCB,(0,QPAY),(5,KILLDATP)                                
*                                                                               
PUBL00   XC    FILTCLT,FILTCLT     CLIENT FILTER                                
         XC    SVLSTN,SVLSTN                                                    
*                                                                               
         CLI   QCLIENT,C' '                                                     
         BNE   PUBL0A                                                           
         CLI   QOPT1,C'V'                                                       
         BNE   *+10                                                             
         MVC   QCLIENT,=C'ALL'                                                  
PUBL0A   DS    0H                                                               
*                                  IF QOPT4 SPECIFIED                           
*                                  ALWAYS SET FILTCLT                           
*                                  UNLESS L = LOCKED FILTER                     
*                               OR UNLESS U = UNLOCKED FILTER                   
         CLI   QOPT4,C' '                                                       
         BE    PUBL0B                                                           
         CLI   QOPT4,C'L'                                                       
         BE    PUBL0B                                                           
         CLI   QOPT4,C'U'                                                       
         BE    PUBL0B                                                           
         CLI   QOPT4,C'N'                                                       
         BE    PUBL0B                                                           
         B     PUBL0C                                                           
PUBL0B   CLC   QPUB+1(2),=C'L='    SEE IF DOING A PUBLIST                       
         BE    PUBL0X              DON'T SET FILTCLT                            
         CLC   QPUB+1(2),=C'P='    SEE IF DOING A PUBLISHER PUBLIST             
         BE    PUBL0X              DON'T SET FILTCLT                            
PUBL0C   CLC   QCLIENT,=C'ALL'                                                  
         BE    PUBL0X                                                           
         CLI   QCLIENT,C' '                                                     
         BE    PUBL0X                                                           
*                                                                               
         CLI   QOPT1,C'V'          SEE IF ADV-AOR PUB LINK LISTING              
         BE    PUBL0X              THEN QCLIENT IS ADV                          
*                                                                               
         MVC   FILTCLT,QCLIENT                                                  
         CLI   QCLIENT,C'*'                                                     
         BNE   *+8                                                              
         MVI   FILTCLT,X'FF'                                                    
*                                                                               
         CLI   FILTCLT,X'FF'      READ CLIENT HEADER                            
         BE    PUBL0X             IF ONE CLIENT SPECIFIED                       
         CLI   FILTCLT,0                                                        
         BE    PUBL0X                                                           
         MVC   MYKEY,KEY                                                        
         MVC   MYKSAVE,KEYSAVE                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),FILTCLT                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                FATAL ERROR                                  
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         MVC   KEY,MYKEY                                                        
         MVC   KEYSAVE,MYKSAVE                                                  
*                                                                               
PUBL0X   DS    0H                 READ FOR P48 PROFILE                          
         XC    PROF48,PROF48                                                    
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'P048'                                                 
         MVC   WORK+4(2),QAGENCY                                                
         MVC   WORK+6(1),QMEDIA                                                 
         CLC   QCLIENT,=C'ALL'                                                  
         BE    PUBL0X4                                                          
         CLC   QCLIENT,SPACES                                                   
         BE    PUBL0X4                                                          
         CLI   QCLIENT,C'*'     SEE IF OFFICE REQUEST                           
         BNE   PUBL0X2                                                          
         MVI   PCLTOFF,C' '      BE SURE OFFICE IS CLEAR                        
*                                                                               
PUBL0X2  DS    0H                                                               
         MVC   WORK+7(3),QCLIENT                                                
         CLI   PCLTOFF,C' '      CHECK FOR CLIENT OFFICE                        
         BNH   *+14                                                             
PUBL0X3  MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
PUBL0X4  DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,PROF48,DATAMGR                                 
         CLI   QOPT1,C'1'       SEE IF ADDRESS LISTING (1-4)                    
         BL    PUBL1                                                            
         CLI   QOPT1,C'4'                                                       
         BH    PUBL1                                                            
         CLC   QCLIENT,=C'ALL'  AND ALL CLIENTS OR NO CLIENT                    
         BE    PUBL0X6                                                          
         CLC   QCLIENT,SPACES                                                   
         BE    PUBL0X6                                                          
         B     PUBL1                                                            
*                                                                               
PUBL0X6  MVI   PROF48,C'N'      CANCEL SPECIAL ADDRESS OPTION                   
*                               (IT WON'T WORK FOR ALL CLTS)                    
PUBL1    DS    0H                                                               
         CLI   MODE,PROCPUB                                                     
         BNE   PUBL4                                                            
*                                                                               
         CLI   QOPT5,C' '         SEE IF FILTERING ON LANGUAGE                  
         BE    PUBL1A                                                           
         OI    PUBLANG,C' '                                                     
         CLI   PUBLANG,C' '                                                     
         BNE   *+8                                                              
         MVI   PUBLANG,C'E'       SINCE BLANK IS ALSO ENGLISH                   
         CLC   PUBLANG,QOPT5                                                    
         BE    PUBL1A                                                           
         B     PLEXT                SKIP THIS PUB                               
*                                                                               
PUBL1A   DS    0H                                                               
         OC    KILLDATE,KILLDATE   SEE IF FILTERING ON KILL DATE                
         BZ    PUBL1A2                                                          
         OC    PUBKILL,PUBKILL     SEE IF I HAVE A KILL DATE                    
         BZ    PUBL1A2             NO - PROCESS                                 
         CLC   PUBKILL,KILLDATE    SKIP THIS PUB                                
         BNH   PLEXT                                                            
*                                                                               
PUBL1A2  DS    0H                                                               
         CLI   QOPT4,C'L'          LOCKED PUBS ONLY                             
         BNE   PUBL1A3                                                          
         TM    PUBLOCSW,PUBLCKDQ                                                
         BNO   PLEXT                                                            
         B     PUBL1A5                                                          
*                                                                               
PUBL1A3  DS    0H                                                               
         CLI   QOPT4,C'U'        UNLOCKED PUBS ONLY                             
         BNE   PUBL1A5                                                          
         TM    PUBLOCSW,PUBLCKDQ                                                
         BO    PLEXT                                                            
         B     PUBL1A5                                                          
*                                                                               
PUBL1A5  DS    0H                                                               
         OC    SCFILTER,SCFILTER   CHECK FOR STATE CODE FILTER                  
         BZ    PUBL1B                                                           
         CLC   PUBSTACD,SCFILTER   STATE CODE MUST MATCH                        
         BNE   PLEXT               SKIP THIS PUB                                
*                                                                               
PUBL1B   CLC   QPUB+1(2),=C'L='    SEE IF DOING A PUBLIST                       
         BNE   PUBL1E                                                           
         CLI   SVLSTN,0            SEE IF NAME SAVED ALREADY                    
         BNE   PUBL1K                                                           
PUBL1C   L     R6,ALISREC                                                       
         USING PLISREC,R6                                                       
         MVC   SVLSTN,PLISDESC     SAVE PUBLIST NAME                            
         DROP  R6                                                               
*                                                                               
PUBL1E   CLC   QPUB+1(2),=C'P='    SEE IF DOING A PUBLISHER PUBLIST             
         BNE   PUBL1K                                                           
         CLI   PBLNAME,0           SEE IF PUBLISHER NAME ALREADY SAVED          
         BNE   PUBL1K                                                           
         MVC   PBLNAME(27),=C'** NOT ON FILE - CALL DDS**'                      
         MVC   MYKEY,KEY                                                        
         MVC   MYKSAVE,KEYSAVE                                                  
         MVC   FULL,AREC                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'11'                                                      
         MVC   KEY+4(4),QPUB+3                                                  
         MVC   KEYSAVE(8),KEY                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BNE   PUBL1F              REP HEADER (PUBLISHER) NOT FOUND             
         LA    R0,PREPREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         MVC   PBLNAME,PREPREC+35  REP (PUBLISHER) NAME                         
PUBL1F   MVC   KEY,MYKEY                                                        
         MVC   KEYSAVE,MYKSAVE                                                  
         MVC   AREC,FULL                                                        
*                                                                               
*                                  FILTER PUBS BY CLIENT/OFFICE                 
PUBL1K   CLI   FILTCLT,0                                                        
         BE    PUBL4                                                            
**NEW 3/26/91                                                                   
         MVC   MYKEY,KEY           TRY AND FIND A BUY FOR THIS CLIENT           
         MVC   MYKSAVE,KEYSAVE                                                  
         MVC   MYDMIN,DMINBTS                                                   
         MVC   MYDMOUT,DMOUTBTS                                                 
         MVI   DMINBTS,X'08'            PASS DELETES                            
         MVI   DMOUTBTS,X'FD'                                                   
***                                                                             
***NOTE IF QOPT4 IS SPECIFIED CHECK FOR X'14' ELEM IS BYPASSED                  
***                                                                             
         CLI   QOPT4,C'Y'          SEE IF DOING ONLY ACTIVE PUBS                
         BE    PUBL1BY                                                          
         CLI   QOPT4,C'B'          SEE IF DOING ONLY ACTIVE PUBS                
         BNE   PUBL1CON                                                         
PUBL1BY  DS    0H                  TRY AND FIND A BUY FOR THIS CLIENT           
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
         MVC   KEY+2(1),PUBKMED                                                 
         MVI   KEY+3,X'21'                                                      
         MVC   KEY+4(3),FILTCLT                                                 
         MVC   KEY+7(6),PUBKPUB                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PUBL1N                                                           
         MVC   KEY,MYKEY             BUY FOUND - PROCESS                        
         MVC   KEYSAVE,MYKSAVE                                                  
         MVC   DMINBTS,MYDMIN                                                   
         MVC   DMOUTBTS,MYDMOUT                                                 
         B     PUBL4                                                            
*                                                                               
PUBL1N   DS    0H                                                               
         CLI   QOPT4,C'Y'              SEE IF LOOKING FOR EITHER                
         BE    PUBL1CON                GO LOOK FOR CONTRACTS                    
PUBL1NX  MVC   KEY,MYKEY                                                        
         MVC   KEYSAVE,MYKSAVE                                                  
         MVC   DMINBTS,MYDMIN                                                   
         MVC   DMOUTBTS,MYDMOUT                                                 
         B     PLEXT                    SKIP THIS PUB                           
*                                                                               
PUBL1CON DS    0H                                                               
         CLI   QOPT4,C'Y'                                                       
         BE    PUBL1C5                                                          
         CLI   QOPT4,C'C'                                                       
         BNE   PUBL1X                                                           
PUBL1C5  XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
         MVC   KEY+2(1),PUBKMED                                                 
         MVI   KEY+3,X'10'                                                      
         MVC   KEY+4(3),FILTCLT                                                 
         CLI   PCLTPROF+5,C'2'       SEE IF SLAVE CLIENT                        
         BNE   PUBL1C8                                                          
         MVC   KEY+4(3),PCLTPROF+6   LOOK UNDER MASTER CLIENT                   
PUBL1C8  MVC   KEY+7(6),PUBKPUB                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PUBL1NX                                                          
         MVC   KEY,MYKEY             BUY FOUND - PROCESS                        
         MVC   KEYSAVE,MYKSAVE                                                  
         MVC   DMINBTS,MYDMIN                                                   
         MVC   DMOUTBTS,MYDMOUT                                                 
         B     PUBL4                                                            
*                                                                               
PUBL1X   DS    0H                                                               
         CLI   QOPT1,C'1'         SEE IF ADDR  QOPT1 1-4                        
         BL    PUBL1X5                                                          
         CLI   QOPT1,C'4'                                                       
         BNH   PUBL4                                                            
*                                                                               
PUBL1X5  CLI   QOPT1,C'D'                                                       
         BE    PUBL4                                                            
         CLC   QSORT,=C'04'                                                     
         BE    PUBL4                                                            
*                                                                               
         LA    R2,PUBREC+33                                                     
PUBL2    DS    0H                                                               
         CLI   0(R2),X'14'                                                      
         BE    PUBL3                                                            
         CLI   0(R2),0                                                          
         BE    PLEXT                                                            
PUBL2B   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     PUBL2                                                            
*                                                                               
PUBL3    DS    0H                                                               
         USING PUBREPEL,R2                                                      
         CLC   FILTCLT,PUBRPOFF                                                 
         BNE   PUBL2B                                                           
*                                                                               
         DROP  R2                                                               
PUBL4    DS    0H                                                               
         CLI   QOPT1,C'P'         PRODUCTION LIST                               
         BE    ONEUP                                                            
         CLI   QOPT1,C'C'          CIRCULATION LIST                             
         BE    ONEUP                                                            
         CLI   QOPT1,C'D'          CLT/DIV/REG/DST                              
         BE    ONEUP                                                            
         CLI   QOPT1,C'$'                                                       
         BE    ONEUP                                                            
         CLI   QOPT1,C'X'                                                       
         BE    ONEUP                                                            
         CLI   QOPT1,C'1'          PAY ADDR                                     
         BE    ONEUP                                                            
         CLI   QOPT1,C'2'          TRA ADDR                                     
         BE    ONEUP                                                            
         CLI   QOPT1,C'3'          CON ADDR                                     
         BE    ONEUP                                                            
         CLI   QOPT1,C'4'          SHIPPING ADDR                                
         BE    ONEUP                                                            
         CLI   QOPT1,C'R'          REPS                                         
*                                  NOTE- REP LOGIC LEFT IN 3-UP CODE            
         BE    ONEUP                                                            
         CLI   QOPT1,C'S'          SAU RATES                                    
         BE    ONEUP                                                            
         CLI   QOPT1,C'L'          LINE EQUIV                                   
         BE    ONEUP                                                            
         CLI   QOPT1,C'W'          WORKSHEET                                    
         BE    ONEUP                                                            
         CLI   QOPT1,C'B'          BUYSHEET                                     
         BE    ONEUP                                                            
         CLI   QOPT1,C'K'          RATE CODES                                   
         BE    ONEUP                                                            
         CLI   QOPT1,C'M'          PUB/CLT STANDARD COMMENTS                    
         BE    ONEUP                                                            
         CLI   QOPT1,C'V'          ADV-AOR ASSIGNMENTS                          
         BE    ONEUP                                                            
         CLI   QOPT1,C'Y'          PAY OPTIONS                                  
         BE    ONEUP                                                            
         CLI   QOPT1,C'G'          PUB GROUP ASSIGNMENTS                        
         BE    ONEUP                                                            
         CLI   QOPT1,C'I'          WEB SITES                                    
         BE    ONEUP                                                            
         B     THREEUP                                                          
*                                                                               
ONEUP    DS    0H                                                               
         GOTO1 =A(PROD),RR=RELO                                                 
         B     PLEXT                                                            
*                                                                               
         SPACE 3                                                                
THREEUP  DS    0H                                                               
*                                                                               
         L     R8,=A(PAGEPOOL)                                                  
         A     R8,RELO                                                          
         SPACE 2                                                                
*              CHECK MODE SETTINGS                                              
         SPACE 3                                                                
         CLI   MODE,FPUBREQ                                                     
         BNE   PL1                                                              
         MVC   PAGE,=H'1'                                                       
         BAS   RE,BLOCKSET                                                      
         MVI   MAXLINES,75                                                      
         B     PLEXT                                                            
*                                                                               
PL1      CLI   MODE,FPUBDST                                                     
         BE    PL2A                                                             
*                                                                               
         SPACE 2                                                                
PL2      CLI   MODE,PROCPUB                                                     
         BNE   PL4                                                              
         CLI   QOPT2,C'Y'      SEE IF DOING AGENCY PUBS ONLY                    
         BNE   PL2A                                                             
         CLC   QAGENCY,PUBKAGY                                                  
         BNE   PLEXT                                                            
*                                                                               
PL2A     LM    R2,R3,0(R8)                                                      
         CR    R2,R3               ANY ROOM FOR ANOTHER ON PAGE                 
         BL    *+8                                                              
         BAS   RE,PRNTPAGE                                                      
         BAS   RE,POSTPUB                                                       
         B     PLEXT                                                            
         SPACE 2                                                                
PL4      CLI   MODE,LPUBDIV                                                     
         BE    PL5                                                              
         CLI   MODE,LPUBREQ                                                     
         BE    PL5                                                              
         CLI   MODE,DISKERR                                                     
         BNE   PLEXT                                                            
         DC    H'0'                                                             
PL5      DS    0H                                                               
         MVI   ENDSW,1             SET FINAL PAGE SWITCH                        
         BAS   RE,PRNTPAGE                                                      
         SPACE 2                                                                
PLEXT    DS    0H                                                               
         CLI   QOPT1,C' '          THREE-UP LISTING ?                           
         BE    PLEXTX              YES                                          
         CLI   QOPT1,C'A'          THREE-UP LISTING ?                           
         BE    PLEXTX              YES                                          
         CLI   QOPT1,C'N'          THREE-UP LISTING ?                           
         BE    PLEXTX              YES                                          
         CLI   QOPT1,C'Z'          THREE-UP LISTING ?                           
         BE    PLEXTX              YES                                          
         CLI   MODE,LPUBDIV                                                     
         BE    PRINTCNT                                                         
         CLI   MODE,LPUBREQ                                                     
         BNE   PLEXTX                                                           
PRINTCNT DS    0H                                                               
         CP    PUBCNT,=P'0'                                                     
         BE    PLEXTX                                                           
         MVC   P(132),SPACES                                                    
         MVC   PSECOND(132),SPACES                                              
         MVC   PSECOND(19),=C'TOTAL PUBLICATIONS='                              
         EDIT  (P3,PUBCNT),(5,PSECOND+19),ALIGN=LEFT                            
         GOTO1 REPORT                                                           
         ZAP   PUBCNT,=P'0'         CLEAR AFTER PRINTING                        
PLEXTX   XMOD1 1                                                                
         EJECT                                                                  
*              SET UP BLOCK VALUES                                              
         SPACE 2                                                                
BLOCKSET XC    0(4,R8),0(R8)                                                    
         MVC   4(4,R8),=F'21'      BLKS PER PAGE                                
         MVC   8(2,R8),=H'49'      LINES PER PAGE                               
         MVC   10(2,R8),=H'252'    BLK SIZE 7 X 36                              
         CLI   QOPT1,C'A'                                                       
         BCR   8,RE                                                             
         SPACE 2                                                                
*****    MVC   4(4,R8),=F'51'                                                   
         MVC   4(4,R8),=F'48'                                                   
*****    MVC   8(2,R8),=H'51'                                                   
         MVC   8(2,R8),=H'48'                                                   
         MVC   10(2,R8),=H'108'                                                 
         CLI   QOPT1,C'Z'                                                       
         BCR   8,RE                                                             
         SPACE 2                                                                
****     MVC   4(4,R8),=F'18'       BLKS PER PG                                 
****     MVC   8(2,R8),=H'48'       LINES PER PG                                
****     MVC   10(2,R8),=H'288'     BLK SIZE 8 X 36                             
****     CLI   QOPT1,C'R'           "OLD" REP LISTING                           
****     BCR   8,RE                 REPS ARE NOW ONE-UP                         
*                                                                               
         CLI   QOPT1,C'E'     NAME,ZONE,ADDR,EDITIONS (FROM PUBEDTS)            
         BER   RE                                                               
*                                                                               
         SPACE 2                                                                
*                          MUST BE NAME LISTING                                 
         MVC   4(4,R8),=F'150'                                                  
         MVC   8(2,R8),=H'50'                                                   
         MVC   10(2,R8),=H'36'                                                  
         CLI   QOPT3,C'Y'          SEE IF DOUBLE SPACING                        
         BNER  RE                                                               
         MVC   4(4,R8),=F'75'      HALVE BLKS PER PAGE                          
         MVC   10(2,R8),=H'72'     DOUBLE BLKSIZE                               
         BR    RE                                                               
         EJECT                                                                  
*              PUT PUBLICATION DETAILS INTO BLOCK                               
         SPACE 2                                                                
POSTPUB  NTR1                                                                   
*                                                                               
PP1      DS    0H                                                               
         MVI   SFLG,X'0'           TURN OFF FLAG                                
         L     R2,0(R8)            ADD 1 TO BLOCKS USED                         
         LA    R3,1(R2)                                                         
         ST    R3,0(R8)                                                         
         MH    R2,10(R8)           DISPLACE TO NEXT SLOT                        
         LA    R2,12(R2,R8)                                                     
         CLI   MODE,PROCPUB                                                     
         BE    PP2                                                              
*                                  CLT/DIV/REG/DST                              
         LR    R5,R2                                                            
         CLC   10(2,R8),=H'36'     TEST 1 LINE PER BLOCK                        
         BNE   PP1B                                                             
         LA    R5,36(R2)                                                        
*                                  CHECK END OF CLOUMN IF                       
*                          1 LINE PER BLOCK                                     
         L     R2,0(R8)                                                         
         BCTR  R2,R0                                                            
         SH    R2,=H'50'                                                        
         BP    *-4                                                              
         C     R2,=F'-3'                                                        
         BL    PP1B                OK                                           
         CLC   0(4,R8),=F'147'                                                  
         BL    PP1                                                              
         BAS   RE,PRNTPAGE         NEW PAGE                                     
         B     PP1                                                              
*                                                                               
PP1B     DS    0H                                                               
         MVC   0(29,R5),=C'CLT/DIV/REG/DST=    /   /   /'                       
         MVC   18(2,R5),PCLTKCLT                                                
         CLI   PCLTKCLT+2,C' '                                                  
         BE    *+10                                                             
         MVC   17(3,R5),PCLTKCLT                                                
         MVC   21(3,R5),PDIVKDIV                                                
         MVC   25(3,R5),PREGKREG                                                
         MVC   29(3,R5),PDSTKDST                                                
         MVI   36(R5),C'-'         UNDERLINE                                    
         MVC   37(31,R5),36(R5)                                                 
*                                                                               
         CLC   10(2,R8),=H'36'     TEST LENGTH OF BLOCK                         
         BNE   XIT                                                              
         L     R2,0(R8)            ADD 3 EXTRA TO BLOCKS USED                   
         LA    R2,3(R2)                                                         
         ST    R2,0(R8)                                                         
         B     XIT                                                              
*                                                                               
PP2      DS    0H                                                               
         MVC   0(20,R2),PUBNAME                                                 
         AP    PUBCNT,=P'1'                                                     
         MVC   WORK,SPACES                                                      
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),PUBKPUB),(C'S',WORK)                          
*                                                                               
         MVC   21(15,R2),WORK                                                   
*                                                                               
         CLI   PUBLANG,C'F'                                                     
         BNE   *+12                                                             
         MVI   20(R2),C'F'                                                      
         MVI   FRSW,1             SO FOOTNOTE WILL PRINT                        
*                                                                               
         CLI   QOPT1,C' '                                                       
         BE    XIT                                                              
         CLI   QOPT1,C'N'                                                       
         BE    XIT                                                              
         CLI   QMEDIA,C'O'         FOR OUTDOOR USE ST,MKT                       
         BNE   PP3                                                              
         LA    RF,36(R2)                                                        
         MVC   0(2,RF),PUBSTACD                                                 
         CLI   PUBSTACD,C' '                                                    
         BNH   *+12                                                             
         CLI   PUBSTACD,C'0'                                                    
         BL    *+10                                                             
         MVC   0(2,RF),PUBSTATE                                                 
         MVI   2(RF),C','                                                       
         MVC   4(20,RF),PUBZNAME                                                
         B     *+10                                                             
PP3      DS    0H                                                               
         MVC   36(20,R2),PUBZNAME                                               
         CLC   WORK+14(6),SPACES                                                
         BE    *+16                                                             
*****    MVC   31(4,R2),SPACES     11/15/96 - BUG IN PRINT O/P                  
*****    MVC   57(10,R2),WORK+10                                                
         MVC   30(6,R2),SPACES     11/15/96 - FIX                               
         MVC   57(10,R2),WORK+09                                                
         CLI   QOPT1,C'Z'                                                       
         BE    XIT                                                              
         OC    36(36,R2),SPACES                                                 
         CLC   36(36,R2),SPACES    IF SECOND LINE IS SPACES SHUFFLE UP          
         BNE   *+8                                                              
         SH    R2,=H'36'                                                        
         MVC   72(30,R2),PUBLINE1                                               
         MVC   108(30,R2),PUBLINE2                                              
         CLI   QMEDIA,C'O'         OUTDOOR ALREADY SHOWS STATE CODE             
         BNE   PP35                                                             
         CLI   PUBSTACD,C'0'       SEE IF NUMERIC                               
         BL    *+10                                                             
*                                  FOR OUTDOOR - ONLY SHOW IF NUMERIC           
PP35     MVC   104(2,R2),PUBSTACD  STATE CODE                                   
*                                                                               
         LA    R5,PUBREC+33                                                     
PP3A     DS    0H                                                               
         CLI   0(R5),X'11'         ADDR ELEM                                    
         BE    PP3A2                                                            
         CLI   0(R5),0                                                          
         BE    PP3B                                                             
         ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     PP3A                                                             
PP3A2    DS    0H                                                               
         USING PUBSADEL,R5                                                      
         CLI   PUBATTN,C' '                                                     
         BNH   PP3A4                                                            
         MVC   144(5,R2),=C'ATTN-'                                              
         MVC   144+6(24,R2),PUBATTN                                             
         MVI   SFLG,X'1'                                                        
PP3A4    CLI   PUBTEL,C' '                                                      
         BNH   PP3A6                                                            
         MVC   180(4,R2),=C'TEL-'                                               
         MVC   180+4(12,R2),PUBTEL                                              
         MVI   SFLG,X'1'                                                        
PP3A6    CLI   1(R5),60            NEW ELEMENT LENGTH?                          
         BNH   PP3B                                                             
         CLI   PUBSFAXN,C' '                                                    
         BNH   PP3B                                                             
         MVC   180+18(4,R2),=C'FAX-'                                            
         MVC   180+22(12,R2),PUBSFAXN                                           
         MVI   SFLG,X'1'                                                        
         CLC   =C'FX=',PUBSFAXN    NUMBER IN CONTROL FILE ?                     
         BNE   PP3B                NO                                           
         MVC   FRSVFAX,PUBSFAXN                                                 
         OC    FRSVFAX,SPACES                                                   
         GOTO1 AGETFAX             GET FAX FROM CONTROL FILE                    
         MVC   180+38(7,R2),=C'FAX NO='                                         
         MVC   180+45(16,R2),TOFAX                                              
PP3B     CLI   SFLG,X'1'                                                        
         BNE   *+8                 NO DATA SO DON'T BUMP                        
         LA    R2,36(R2)           BUMP TO NEXT BLK                             
         DS    0H                                                               
         CLI   QOPT1,C'A'                                                       
         BE    XIT                                                              
         CLI   PUBEDTS,C' '                                                     
         BNH   PP6                                                              
         CLI   PUBTEL,C' '                                                      
         BNH   PP40                                                             
         MVC   216(4,R2),=C'TEL-'                                               
         MVC   216+4(12,R2),PUBTEL                                              
PP40     CLI   PUBSFAXN,C' '                                                    
         BNH   PP41                                                             
         MVC   216+19(4,R2),=C'FAX-'                                            
         MVC   216+23(12,R2),PUBSFAXN                                           
         DROP  R5                                                               
PP41     MVC   144(4,R2),=C'EDS='                                               
         LA    R6,147(R2)                                                       
         LA    R5,PUBEDTS                                                       
         LA    R0,15                                                            
PP4      DS    0H                                                               
         CLI   0(R5),C' '                                                       
         BNH   PP5                                                              
         MVC   WORK(5),=X'9999999900'                                           
         MVC   WORK+5(1),0(R5)                                                  
         GOTO1 PUBEDIT,DMCB,WORK,WORK+6                                         
*                                                                               
         MVC   2(3,R6),WORK+15                                                  
         LA    R6,2(R6)                                                         
         CLI   0(R6),C' '                                                       
         BH    *+8                                                              
         BCT   R6,*-8                                                           
         MVI   1(R6),C','                                                       
         LA    R5,1(R5)                                                         
         BCT   R0,PP4                                                           
PP5      DS    0H                                                               
         MVI   1(R6),C' '                                                       
PP6      DS    0H                                                               
         CLI   QOPT1,C'E'                                                       
         BE    XIT                                                              
         MVC   144(36,R2),SPACES                                                
         MVC   144(25,R2),=C'REPS P=NONE T=NONE C=NONE'                         
         LA    R4,PUBREC+33        LOOK FOR REP ELEMENT                         
         SR    R5,R5                                                            
         SPACE 2                                                                
POST2    CLI   0(R4),0                                                          
         BE    POST6                                                            
         CLI   0(R4),X'14'                                                      
         BE    POST4                                                            
         IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     POST2                                                            
         SPACE 2                                                                
POST4    EQU   *                                                                
         USING PUBREPEL,R4                                                      
         OC    PUBPAREP,=4C'0'                                                  
         CLC   PUBPAREP,=4C'0'                                                  
         BE    *+10                                                             
         MVC   151(4,R2),PUBPAREP                                               
         OC    PUBTRREP,=4C'0'                                                  
         CLC   PUBTRREP,=4C'0'                                                  
         BE    *+10                                                             
         MVC   158(4,R2),PUBTRREP                                               
         OC    PUBCNREP,=4C'0'                                                  
         CLC   PUBCNREP,=4C'0'                                                  
         BE    *+10                                                             
         MVC   165(4,R2),PUBCNREP                                               
         SPACE 2                                                                
POST6    EQU   *                                                                
         B     POST12              NOP AC/CD ON 3-UP LIST                       
POST12   EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
*              PRINT A PAGE THREE UP                                            
         SPACE 3                                                                
PRNTPAGE NTR1                                                                   
*                                                                               
         OC    0(4,R8),0(R8)                                                    
         BZ    XIT                                                              
*                                                                               
         CLI   FILTCLT,0        SEE IF USING CLIENT FILTER                      
         BE    PRNT2X                                                           
         CLI   QCLIENT,C'*'                                                     
         BNE   PRNT2                                                            
         MVC   HEAD4+1(7),=C'OFFICE='                                           
*SMY*    MVC   HEAD4+9(1),QCLIENT+1                                             
*                                                                               
*        PRINT OFFICE CODE                                                      
*                                                                               
         GOTOR VPRNTOFC,DMCB,QCLIENT+1,(C'L',HEAD4+10),VOFFICER,       X        
               QAGENCY,VCOMFACS                                                 
*                                                                               
         B     PRNT2X                                                           
*                                                                               
PRNT2    MVC   HEAD4+1(7),=C'CLIENT='                                           
         MVC   HEAD4+9(3),QCLIENT                                               
*                                                                               
PRNT2X   DS    0H                                                               
         CLI   QOPT4,C'Y'                                                       
         BNE   *+10                                                             
         MVC   HEAD5+1(36),=C'* PUBS WITH BUYS OR CONTRACTS ONLY *'             
         CLI   QOPT4,C'B'                                                       
         BNE   *+10                                                             
         MVC   HEAD5+1(23),=C'* PUBS WITH BUYS ONLY *'                          
         CLI   QOPT4,C'C'                                                       
         BNE   *+10                                                             
         MVC   HEAD5+1(28),=C'* PUBS WITH CONTRACTS ONLY *'                     
         CLI   QOPT4,C'L'                                                       
         BNE   *+10                                                             
         MVC   HEAD5+1(20),=C'* LOCKED PUBS ONLY *'                             
         CLI   QOPT4,C'U'                                                       
         BNE   *+10                                                             
         MVC   HEAD5+1(22),=C'* UNLOCKED PUBS ONLY *'                           
*                                                                               
         CLI   QOPT5,C'E'     LANGUAGE FILTER                                   
         BNE   *+10                                                             
         MVC   HEAD6+1(30),=C'* ENGLISH LANGUAGE PUBS ONLY *'                   
         CLI   QOPT5,C'F'                                                       
         BNE   *+10                                                             
         MVC   HEAD6+1(29),=C'* FRENCH LANGUAGE PUBS ONLY *'                    
*                                                                               
PRNT3    CLC   QPUB+1(2),=C'L='    PUB LIST                                     
         BNE   PRNT3B                                                           
         MVC   HEAD6+43(9),=C'PUB LIST='                                        
         MVC   HEAD6+53(3),QPUB+3                                               
         MVC   HEAD6+57(20),SVLSTN    SAVED PUBLIST NAME                        
*                                                                               
PRNT3B   CLC   QPUB+1(2),=C'P='    PUBLISHER PUB LIST                           
         BNE   PRNT4                                                            
         MVC   HEAD6+39(10),=C'PUBLISHER='                                      
         MVC   HEAD6+49(4),QPUB+3                                               
         MVC   HEAD6+55(30),PBLNAME   SAVED PUBLISHER NAME                      
*                                                                               
PRNT4    OC    KILLDATE,KILLDATE                                                
         BZ    PRNT5                                                            
         MVC   HEAD7+1(52),=C'* NO PUBS WHOSE KILL DATE IS ON OR BEFOREX        
                         *'                                                     
         MVC   HEAD7+43(8),KILLDATP                                             
*                                                                               
PRNT5    XC    0(4,R8),0(R8)                                                    
         L     R2,4(R8)                                                         
         SRDA  R2,32                                                            
         D     R2,=F'3'                                                         
         LR    R2,R3                                                            
         LH    R3,8(R8)            R2=DISPLACEMENT                              
*                                  R3=NUMBER OF LINES TO PRINT                  
         MH    R2,10(R8)                                                        
         LA    R4,12(R8)           R4,R5,R6 ADDRESS PUB BLOCKS                  
         LA    R5,0(R2,R4)                                                      
         LA    R6,0(R2,R5)                                                      
         MVI   FORCEHED,C'Y'                                                    
         SPACE 2                                                                
PRNT10   MVC   P(132),SPACES                                                    
         MVC   P+1(36),0(R4)                                                    
         MVC   P+37(36),0(R5)                                                   
         MVC   P+73(36),0(R6)                                                   
         MVI   RCSUBPRG,0                                                       
         GOTO1 REPORT                                                           
         MVC   0(36,R4),SPACES                                                  
         MVC   0(36,R5),SPACES                                                  
         MVC   0(36,R6),SPACES                                                  
         LA    R4,36(R4)                                                        
         LA    R5,36(R5)                                                        
         LA    R6,36(R6)                                                        
         BCT   R3,PRNT10                                                        
*                                                                               
         CLI   ENDSW,1             PRINTING FINAL PAGE ?                        
         BNE   PRNTFR              NO                                           
         MVC   P(132),SPACES                                                    
         MVC   PSECOND(132),SPACES                                              
         MVC   PSECOND(19),=C'TOTAL PUBLICATIONS='                              
         EDIT  (P3,PUBCNT),(5,PSECOND+19),ALIGN=LEFT                            
         GOTO1 REPORT                                                           
         MVI   ENDSW,0             TURN OFF END INDICATOR                       
*                                                                               
PRNTFR   CLI   FRSW,1           SEE IF A FRENCH PUB ON THIS PAGE                
         BNE   PRNTPX                                                           
         GOTO1 REPORT           SKIP A LINE                                     
         MVC   P(70),=C'NOTE - "F" BEFORE THE PUB NUMBER DENOTES A FRENX        
               CH LANGUAGE PUBLICATION'                                         
         GOTO1 REPORT                                                           
         MVI   FRSW,0     SO FOOTNOTE ONLY PRINTS ON PAGES                      
*                         WHERE THE "F" APPEARS BEFORE THE PUB NO.              
         SPACE 2                                                                
PRNTPX   DS    0H                                                               
*                                                                               
XIT      XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
FRSW     DS    X                                                                
ENDSW    DS    X                   END OF REPORT INDICATOR                      
SFLG     DS    X                                                                
MYKEY    DS    CL32                                                             
MYKSAVE  DS    CL32                                                             
MYDMIN   DS    CL1                                                              
MYDMOUT  DS    CL1                                                              
*                                                                               
         EJECT                                                                  
PROD     CSECT                                                                  
         NMOD1 0,**PROD                                                         
         SPACE 2                                                                
         LA    R8,1(RB)                                                         
         LA    R8,4095(R8)                                                      
         USING PROD+4096,R8     ** 2ND BASE REG **                              
         LA    R3,1(R8)                                                         
         LA    R3,4095(R3)                                                      
         USING PROD+8192,R3     ** 3RD BASE REG **                              
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC,R9                                                    
         USING PP48WRKD,R7                                                      
*                                                                               
*                                                                               
*                                                                               
         L     RF,PPWORK2C         ADDRESS OF PPREPWORK2C                       
         USING PPWORK2D,RF                                                      
         MVC   ACONIO1,ACONIO      STORE ADDRESS                                
         DROP  RF                                                               
*                                                                               
*                                                                               
*                                                                               
         CLI   MODE,FPUBREQ                                                     
         BNE   PRODA                                                            
         MVC   PAGE,=H'1'                                                       
         MVI   MAXLINES,60                                                      
         MVI   LNEED,0                                                          
         MVI   FORCEHED,C'Y'                                                    
         ZAP   PCOUNT,=P'999999999'                                             
         CP    RCSPECNO,=P'1'                                                   
         BNH    *+10                                                            
         ZAP   PCOUNT,RCSPECNO                                                  
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         CLI   QMEDIA,C'N'                                                      
         BNE   PREXT                                                            
         CLI   QOPT1,C'P'          FOR NEWSPAPER PROD                           
         BNE   PREXT                                                            
         MVI   FCGTPUB,C'B'        SET TO READ 'LITTLE' RECORD                  
         B     PREXT                                                            
*                                                                               
*                                                                               
*                                                                               
ACONIO1  DS    F                   SAVE CONTRACT RECORD IN THIS CSECT           
*                                                                               
*                                                                               
*                                                                               
PRODA    CLI   MODE,FPUBDST                                                     
         BNE   PRODB                                                            
         LA    R5,MID1                                                          
         MVC   0(29,R5),=C'CLT/DIV/REG/DST=    /   /   /'                       
         MVC   18(2,R5),PCLTKCLT                                                
         CLI   PCLTKCLT+2,C' '                                                  
         BE    *+10                                                             
         MVC   17(3,R5),PCLTKCLT                                                
         MVC   21(3,R5),PDIVKDIV                                                
         MVC   25(3,R5),PREGKREG                                                
         MVC   29(3,R5),PDSTKDST                                                
         LA    R5,132(R5)                                                       
         MVI   0(R5),C'-'                                                       
         MVC   1(31,R5),0(R5)                                                   
*                                                                               
         MVI   FORCEMID,C'Y'                                                    
         B     PREXT                                                            
*                                                                               
PRODB    DS    0H                                                               
PROD1    CLI   MODE,PROCPUB                                                     
         BNE   PREXT                                                            
         CLI   QOPT2,C'Y'                                                       
         BNE   PROD1A                                                           
         CLC   QAGENCY,PUBKAGY                                                  
         BNE   PREXT                                                            
*                                                                               
PROD1A   DS    0H                                                               
         L     RF,ALTLREC                                                       
         CLC   PUBKEY(7),0(RF)                                                  
         BE    *+10                                                             
         XC    0(50,RF),0(RF)      ENSURE NOT USING WRONG LTLREC                
         XC    P,P                                                              
         XC    PSECOND,PSECOND                                                  
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),PUBKPUB),P                                    
         MVC   P+18(20),PUBNAME                                                 
         AP    PUBCNT,=P'1'        1-UP COUNT OF PUBS                           
         CLI   QMEDIA,C'O'                                                      
         BNE   PROD1B                                                           
*                                  FOR OUTDOOR USE ST,MKT FOR CITY              
         LA    RF,P+39                                                          
         MVC   0(2,RF),PUBSTACD                                                 
         CLI   PUBSTACD,C' '                                                    
         BNH   *+12                                                             
         CLI   PUBSTACD,C'0'                                                    
         BL    *+10                                                             
         MVC   0(2,RF),PUBSTATE                                                 
         MVI   2(RF),C','                                                       
         MVC   4(20,RF),PUBZNAME                                                
         XC    PUBZNAME,PUBZNAME                                                
         B     PROD1D                                                           
PROD1B   DS    0H                                                               
         MVC   P+39(16),PUBCITY                                                 
         MVC   P+57(2),PUBSTATE                                                 
         MVC   P+60(2),PUBSTACD    STATE CODE                                   
PROD1D   DS    0H                                                               
*                                                                               
         CLI   QOPT1,C'C'                                                       
         BE    CIRC                                                             
         CLI   QOPT1,C'D'                                                       
         BE    CDRD                                                             
         CLI   QOPT1,C'$'                                                       
         BE    RATE                                                             
         CLI   QOPT1,C'X'                                                       
         BE    PREM                                                             
         CLI   QOPT1,C'R'                                                       
         BE    REP0                                                             
         CLI   QOPT1,C'1'                                                       
         BE    ADDR                                                             
         CLI   QOPT1,C'2'                                                       
         BE    ADDR                                                             
         CLI   QOPT1,C'3'                                                       
         BE    ADDR                                                             
         CLI   QOPT1,C'4'                                                       
         BE    ADDR                                                             
         CLI   QOPT1,C'L'                                                       
         BE    CLE                                                              
         CLI   QOPT1,C'S'          SAU RATES                                    
         BE    RATE                                                             
         CLI   QOPT1,C'K'          RATE CODES                                   
         BE    RATE                                                             
         CLI   QOPT1,C'M'          PUB/CLT STANDARD COMMENTS                    
         BE    REP0                MOSTLY SAME AS REP                           
         CLI   QOPT1,C'V'          ADV- AOR ASSIGNMENTS                         
         BE    ADV                                                              
         CLI   QOPT1,C'Y'          PAY OPTIONS                                  
         BE    PAY                                                              
         CLI   QOPT1,C'G'          PUB GROUP ASSIGNMENTS                        
         BE    PGA                                                              
*                                                                               
*                    MUST BE QOPT1=P,W,B OR I                                   
PROD1F   MVI   RCSUBPRG,2                                                       
         CLI   QMEDIA,C'N'                                                      
         BE    *+8                                                              
         MVI   RCSUBPRG,3                                                       
         CLI   QOPT1,C'W'          WORKSHEET                                    
         BNE   PROD1G                                                           
         MVI   RCSUBPRG,1                                                       
         B     PROD1H                                                           
PROD1G   CLI   QOPT1,C'B'          BUYSHEET                                     
         BNE   PROD1H                                                           
         MVI   RCSUBPRG,16                                                      
PROD1H   LA    R4,PUBREC+33                                                     
         SR    R5,R5                                                            
*                                                                               
         CLI   PUBLANG,C'F'         SEE IF FRENCH                               
         BNE   *+10                                                             
         MVC   PSECOND(6),=C'FRENCH'                                            
*                                                                               
         OC    PUBZNAME,PUBZNAME                                                
         BZ    PROD2                                                            
         MVC   PSECOND+18(20),PUBZNAME                                          
PROD2    DS    0H                                                               
         CLI   QOPT1,C'W'          WORKSHEET                                    
         BE    PRINTIT             NOTHING ELSE TO PRINT                        
         CLI   QOPT1,C'B'          BUYSHEET                                     
         BE    PRINTIT             NOTHING ELSE TO PRINT                        
         CLI   QOPT1,C'I'          WEB SITE ?                                   
         BNE   PROD2AA             NO - MUST BE PRODUCTION                      
*                                                                               
         MVI   ELCODE1,X'70'       TRY AND FIND WEB SITE ELEM                   
         BAS   RE,NEXTEL                                                        
         BE    PRODWEB                                                          
         SP    PUBCNT,=P'1'        "ADJUST" COUNT OF PUBS                       
         B     PREXT               NOTHING TO PRINT                             
*                                                                               
PRODWEB  ZIC   RE,1(R4)            LENGTH OF WEB SITE ELEM                      
         SH    RE,=H'3'            PREP FOR EXECUTED MOVE                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   P+63(0),2(R4)   EXECUTED MOVE                                    
         MVI   RCSUBPRG,26                                                      
         B     PRINTIT             NOTHING ELSE TO PRINT                        
*                                                                               
PROD2AA  OC    PUBKILL,PUBKILL                                                  
         BZ    PROD2A                                                           
*        GOTO1 DTCNV,DMCB,(1,PUBKILL),(3,P+109)                                 
         GOTO1 DATCON,DMCB,(3,PUBKILL),(5,P+109)                                
*                                                                               
PROD2A   MVI   ELCODE1,X'20'     TRY AND FIND PRODUCTION ELEM                   
         BAS   RE,NEXTEL                                                        
         BE    PROD3                                                            
         CLI   QMEDIA,C'N'        IF NEWSPAPERS                                 
         BE    PROD4              STILL SEARCH FOR PUBSPREL                     
         B     PRINTIT                                                          
*                                                                               
PROD3    EQU   *                                                                
         USING PUBGENEL,R4                                                      
**NEW 3/9/89                                                                    
         CP    PUBAC,=P'-1'      -.001 MEANS 100.00                             
         BNE   PROD3A                                                           
         MVC   P+63(7),=C'100.000'                                              
         B     PROD3B                                                           
**NEW 3/9/89                                                                    
PROD3A   EDIT  (P3,PUBAC),(6,P+63),3                                            
PROD3B   EDIT  (P2,PUBCD),(4,P+70),1                                            
         EDIT  (P2,PUBCDDAS),(3,P+75),0                                         
         EDIT  (P2,PUBCLMO),(3,P+81),0,FLOAT=-                                  
         EDIT  (P2,PUBCLDA),(3,P+85),0,FLOAT=-                                  
*                                                                               
         DS    0H                                       **************          
         CLC   PUBMCLMO(4),=X'000C000C'                 ADD 12/11/87 *          
         BE    PROD2B                                                *          
         LA    R5,PSECOND+81                                         *          
         MVC   PSECOND+76(5),=C'MAT.='                               *          
         EDIT  (P2,PUBMCLMO),(3,(R5)),0,FLOAT=-                      *          
         EDIT  (P2,PUBMCLDA),(3,4(R5)),0,FLOAT=-        **************          
*                                                                               
PROD2B   DS    0H                                                               
         OC    PUBCDDAT,PUBCDDAT   CHK FOR CD EFFECTIVE DATE                    
*NOP*    BZ    PROD2D                                                           
         BZ    PROD2C                                                           
*NOP*    GOTO1 DATCON,DMCB,(3,PUBCDDAT),(5,PSECOND+68)                          
         GOTO1 DATCON,DMCB,(3,PUBCDDAT),(5,PSECOND+66)                          
*                                                                               
PROD2C   DS    0H                                   *** NEW A/O 11/99           
         OC    PUBEXCL,PUBEXCL     CHECK FOR EXCLUSIONS                         
         BZ    PROD2D              NO EXCLUSIONS                                
         MVC   PSECOND+90(11),=C'EXCLUSIONS='                                   
         LA    R5,PSECOND+102                                                   
         TM    PUBEXCL,X'20'                                                    
         BZ    PROD2CB                                                          
         MVC   0(7,R5),=C'LIQUOR,'                                              
         LA    R5,7(R5)                                                         
         SPACE 2                                                                
PROD2CB  TM    PUBEXCL,X'10'                                                    
         BZ    PROD2CD                                                          
         MVC   0(8,R5),=C'TOBACCO,'                                             
         LA    R5,8(R5)                                                         
         SPACE 2                                                                
PROD2CD  TM    PUBEXCL,X'80'                                                    
         BZ    PROD2CF                                                          
         MVC   0(5,R5),=C'BEER,'                                                
         LA    R5,5(R5)                                                         
         SPACE 2                                                                
PROD2CF  TM    PUBEXCL,X'40'                                                    
         BZ    PROD2CH                                                          
         MVC   0(5,R5),=C'WINE,'                                                
         LA    R5,5(R5)                                                         
         SPACE 2                                                                
PROD2CH  TM    PUBEXCL,X'08'                                                    
         BZ    PROD2CX                                                          
         LA    RE,PSECOND+122                                                   
         CR    RE,R5               ROOM FOR FULL WORD CIGARETTES ?              
         BNL   PROD2CH4            YES                                          
         MVC   0(05,R5),=C'CIGS.'  USE ABBREVIATION                             
         B     PROD2D                                                           
PROD2CH4 MVC   0(10,R5),=C'CIGARETTES'                                          
         B     PROD2D                                                           
PROD2CX  DS    0H                  CLEAR TRAILING COMMA                         
         BCTR  R5,0                "LEFT" 1 SPACE                               
         CLI   0(R5),C','          TRAILING COMMA ?                             
         BNE   PROD2D              NO                                           
         MVI   0(R5),C' '          CLEAR IT                                     
         SPACE 2                                                                
PROD2D   DS    0H                                                               
         CLI   QMEDIA,C'N'                                                      
         BE    NEWSFLDS                                                         
         EDIT  (P2,PUBPAYMO),(3,P+89),0,FLOAT=-                                 
         TM    PUBPAYDA,X'F0'                                                   
         BZ    PROD2F                                                           
         NI    PUBPAYDA,X'0F'                                                   
         EDIT  (P2,PUBPAYDA),(3,P+93),0,FLOAT=+                                 
         B     PROD2G                                                           
*                                                                               
PROD2F   EDIT  (P2,PUBPAYDA),(3,P+93),0,FLOAT=-                                 
PROD2G   EDIT  (P2,PUBOSMO),(3,P+99),0,FLOAT=-                                  
         EDIT  (P2,PUBOSDA),(3,P+103),0,FLOAT=-                                 
         MVC   P+121(3),PUBMCLAS                                                
         MVC   P+127(2),PUBMFREQ                                                
         B     PRINTIT                                                          
*                                                                               
NEWSFLDS EDIT  (P2,PUBCPP),(3,P+89),0                                           
         CLI   PUBLPCI,C'I'        SEE IF INCHES                                
         BNE   NWSF1                                                            
         XC    DOUBLE,DOUBLE                                                    
         MVC   DOUBLE+5(2),PUBLPC                                               
         MVI   DOUBLE+7,X'0C'                                                   
         DP    DOUBLE,=P'10'                                                    
         ZAP   DOUBLE,DOUBLE(6)                                                 
         EDIT  (P8,DOUBLE),(6,P+93),2,TRAIL=C'I'                                
         B     NWSF1C                                                           
NWSF1    EDIT  (P2,PUBLPC),(4,P+94),0,TRAIL=C'L'                                
*                                                                               
NWSF1C   CLI   PUBFLAT,C' '                                                     
         BNH   NWSF2                                                            
         MVC   P+118(4),=C'FLAT'                                                
         CLI   PUBFLAT,C'F'                                                     
         BE    NWSF2                                                            
         MVC   P+118(4),=C' SS '                                                
NWSF2    DS    0H                                                               
         OC    PUBFD,PUBFD         SEE IF FULL DEPTH THERE                      
         BZ    NWSF4                                                            
         EDIT  (P3,PUBFD),(5,P+123),2                                           
NWSF4    DS    0H                                                               
         LA    R1,P+100                                                         
         LA    R6,BFDTAB                                                        
*****    LA    R3,3         TO ALLOW R3 AS THIRD BASE FOR PROD CSECT            
         LA    R0,3                                                             
CKBITS   MVC   WORK(1),PUBBFDS                                                  
         NC    PUBBFDS(1),0(R6)                                                 
         CLC   PUBBFDS,WORK                                                     
         BE    NEXTBTF                                                          
         MVC   0(2,R1),1(R6)                                                    
*****    BCT   R3,*+8                                                           
         BCT   R0,*+8                                                           
         B     PROD4                                                            
         OC    PUBBFDS,PUBBFDS                                                  
         BZ    PROD4                                                            
         MVI   2(R1),C'-'                                                       
         LA    R1,3(R1)                                                         
*                                                                               
NEXTBTF  CLC   3(2,R6),=X'0000'                                                 
         BE    PROD4                                                            
         LA    R6,3(R6)                                                         
         OC    PUBBFDS,PUBBFDS                                                  
         BZ    PROD4                                                            
         B     CKBITS                                                           
*                                                                               
*                  M O   T U   W E   T H   F R   S A   S U                      
BFDTAB   DC    X'BFD4D6DFE3E4EFE6C5F7E3C8FBC6D9FDE2C1FEE2E40000'                
*                                                                               
PROD4    DS    0H                                                               
         L     R4,ALTLREC                                                       
         LA    R4,33(R4)                                                        
PROD4B   DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    PROD5                                                            
         CLI   0(R4),X'21'         SUPPL PROD INFO                              
         BE    PROD4D                                                           
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     PROD4B                                                           
PROD4D   DS    0H                                                               
         USING PUBSPREL,R4                                                      
         MVC   P+129(2),PUBCLASS                                                
PROD5    DS    0H                                                               
         B     PRINTIT                                                          
         SPACE 2                                                                
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
CIRC     DS    0H                                                               
         MVI   INFSW,0                                                          
         MVI   RCSUBPRG,4                                                       
         MVC   PSECOND,SPACES                                                   
         CLI   QMEDIA,C'N'                                                      
         BE    *+8                                                              
         MVI   RCSUBPRG,5                                                       
         LA    R4,PUBREC+33                                                     
         SR    R5,R5                                                            
CIRC1    CLI   0(R4),0                                                          
         BE    CIRCX                                                            
         CLI   0(R4),X'30'                                                      
         BE    CIRC2                                                            
CIRC1B   IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     CIRC1                                                            
*                                                                               
CIRC2    EQU   *                                                                
         MVI   INFSW,1                                                          
         USING PUBCIREL,R4                                                      
         EDIT  PUBCIR1,(10,P+63),0,COMMAS=YES                                   
*                                                                               
CIRC4    DS    0H                                                               
         OC    PUBCDAT,PUBCDAT                                                  
         BZ    CIRC5                                                            
*        GOTO1 DTCNV,DMCB,(1,PUBCDAT),(3,P+78)                                  
         GOTO1 DATCON,DMCB,(3,PUBCDAT),(5,P+78)                                 
*                                                                               
         CLC   P+78+3(2),=C'00'                                                 
         BNE   *+16                                                             
         MVC   P+78+3(3),P+78+5                                                 
         MVC   P+78+6(2),SPACES                                                 
CIRC5    DS    0H                                                               
         MVC   P+88(4),PUBCSRC                                                  
CIRC5B   CLC   P+18(20),SPACES     SEE IF PUBNAME IN P                          
         BNE   CIRC5C                                                           
*                                                                               
         CLI   PUBLANG,C'F'         SEE IF FRENCH                               
         BNE   *+10                                                             
         MVC   P(6),=C'FRENCH'                                                  
*                                                                               
         MVC   P+18(20),PUBZNAME                                                
         MVC   PUBZNAME,SPACES                                                  
CIRC5C   BAS   RE,PRINTITR            RETURNS                                   
         B     CIRC1B                                                           
*                                                                               
*                                                                               
CIRCX    CLI   INFSW,1             SEE IF CIRCS PRINTED                         
         BE    CIRCXX                                                           
         MVI   PSECOND,0           SO PSECOND WILL PRINT                        
*                                                                               
         CLI   PUBLANG,C'F'         SEE IF FRENCH                               
         BNE   *+10                                                             
         MVC   PSECOND(6),=C'FRENCH'                                            
*                                                                               
         MVC   PSECOND+18(20),PUBZNAME                                          
         B     PRINTIT             SO I'LL PRINT ALL PUBS                       
*                                                                               
*                                                                               
CIRCXX   CLC   PUBZNAME,SPACES     SEE IF PUBZNAME PRINTED                      
         BE    DSPCE                                                            
         CLI   PUBLANG,C'F'         SEE IF FRENCH                               
         BNE   *+10                                                             
         MVC   P(6),=C'FRENCH'                                                  
*                                                                               
         MVC   P+18(20),PUBZNAME                                                
         B     PRINTIT                                                          
         EJECT                                                                  
CDRD     DS    0H                  CLT/DIV/REG/DST                              
         MVI   INFSW,0                                                          
         MVC   PSECOND,SPACES                                                   
         MVI   RCSUBPRG,6                                                       
         L     R4,ALTLREC                                                       
         LA    R4,33(R4)                                                        
CDRD2    CLI   0(R4),0                                                          
         BE    CDRDX               DONE                                         
         CLI   0(R4),X'71'                                                      
         BE    CDRD4                                                            
CDRD3    DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     CDRD2                                                            
CDRD4    DS    0H                                                               
         USING PUBDSTEL,R4                                                      
         CLC   QCLIENT,SPACES                                                   
         BE    CDRD6                                                            
         CLC   QCLIENT,=C'ALL'                                                  
         BE    CDRD6                                                            
         CLC   QCLIENT,PUBDCLT                                                  
         BNE   CDRD3                                                            
CDRD6    CLC   QDIV,SPACES                                                      
         BE    CDRD7                                                            
         CLC   QDIV,=C'ALL'                                                     
         BE    CDRD7                                                            
         CLC   QDIV,PUBDDIV                                                     
         BNE   CDRD3                                                            
CDRD7    CLC   QREGION,SPACES                                                   
         BE    CDRD8                                                            
         CLC   QREGION,=C'ALL'                                                  
         BE    CDRD8                                                            
         CLC   QREGION,PUBDREG                                                  
         BNE   CDRD3                                                            
CDRD8    CLC   QDIST,SPACES                                                     
         BE    CDRD9                                                            
         CLC   QDIST,=C'ALL'                                                    
         BE    CDRD9                                                            
         CLC   QDIST,PUBDDST                                                    
         BNE   CDRD3                                                            
CDRD9    DS    0H                                                               
         MVI   INFSW,1                                                          
         LA    R5,P+65                                                          
         MVC   00(3,R5),PUBDCLT                                                 
         MVC   05(3,R5),PUBDDIV                                                 
         MVC   10(3,R5),PUBDREG                                                 
         MVC   15(3,R5),PUBDDST                                                 
         OC    PUBDSHR(2),PUBDSHR                                               
         BZ    CDRD10                                                           
         LA    R5,19(R5)                                                        
         EDIT  (B2,PUBDSHR),(6,0(R5)),2                                         
*                                                                               
CDRD10   DS    0H                                                               
         DROP  R4                                                               
         CLC   P+18(20),SPACES     SEE IF PUBNAME IN P                          
         BNE   CDRD12                                                           
*                                                                               
         CLI   PUBLANG,C'F'         SEE IF FRENCH                               
         BNE   *+10                                                             
         MVC   P(6),=C'FRENCH'                                                  
*                                                                               
         MVC   P+18(20),PUBZNAME                                                
         MVC   PUBZNAME,SPACES                                                  
CDRD12   DS    0H                                                               
         BAS   RE,PRINTITR                                                      
         B     CDRD3                                                            
*                                                                               
CDRDX    CLI   QOPT4,C'Y'         SEE IF IF DOING ACTIVITY                      
         BE    CDRDX5                                                           
         CLI   QOPT4,C'B'         BUYS                                          
         BE    CDRDX5                                                           
         CLI   QOPT4,C'C'         CONTRACTS                                     
         BE    CDRDX5             YES - ALWAYS PRINT PUB                        
*                                                                               
         CLI   INFSW,1             SEE IF I PRINTED INFO                        
         BE    CDRDX5                                                           
         SP    PUBCNT,=P'1'        ADJUST PUB COUNT                             
         B     PREXT                                                            
*                                                                               
CDRDX5   CLC   PUBZNAME,SPACES     SEE IF PUBZNAME PRINTED                      
         BE    DSPCE                                                            
*                                                                               
         CLI   PUBLANG,C'F'         SEE IF FRENCH                               
         BNE   *+10                                                             
         MVC   P(6),=C'FRENCH'                                                  
*                                                                               
         MVC   P+18(20),PUBZNAME                                                
         B     PRINTIT                                                          
         EJECT                                                                  
*                                  RATE LIST                                    
         SPACE 2                   PRINT OUT RATE CODES OR SAUS                 
RATE     DS    0H                                                               
         MVI   INFSW,0                                                          
*                                                                               
         CLI   QOPT1,C'S'          IF SAUS                                      
         BNE   RAT1                                                             
         MVC   MYSVMED,QMEDIA      SAVE REQUESTED MEDIA                         
*                                  WILL BE RESET AT RAT38                       
         MVI   QMEDIA,C'M'         PRETEND IT'S MAGAZINES                       
         B     RAT1A                                                            
*                                                                               
RAT1     CLI   QOPT1,C'K'          IF RATE CODES                                
         BNE   RAT1A                                                            
         MVC   MYSVMED,QMEDIA      SAVE REQUESTED MEDIA                         
*                                  WILL BE RESET AT RAT38                       
         MVI   QMEDIA,C'M'         PRETEND IT'S MAGAZINES                       
         MVI   RCSUBPRG,17         MOVE IN SPROG FOR RATE CODES.                
         B     RAT1B                                                            
*                                                                               
RAT1A    MVI   RCSUBPRG,7                                                       
         CLI   QMEDIA,C'N'                                                      
         BE    *+8                                                              
         MVI   RCSUBPRG,11                                                      
RAT1B    MVC   PSECOND,SPACES                                                   
         LA    R5,P                                                             
         USING RTLIND,R5                                                        
         LA    R4,PUBREC+33                                                     
RAT2     DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    RAT30                                                            
         CLI   0(R4),X'50'                                                      
         BE    RAT10                                                            
         CLI   0(R4),X'51'                                                      
         BE    RAT20                                                            
RAT4     DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     RAT2                                                             
*                                  RATE ELEM                                    
RAT10    DS    0H                                                               
         USING PUBRATEL,R4                                                      
*****                                                                           
         CLC   QSTART,SPACES               QSTART HOLDS FILTER DATE ON          
         BE    RAT11                              EFFECTIVE DATE                
         GOTO1 DATCON,DMCB,(3,PUBRSTRT),(0,DUB)                                 
         CLC   QSTART(4),DUB                      IS FILTER DATE(Y/M)           
         BNE   RAT4                               = TO EFFECTIVE DATE           
*****                                                                           
RAT11    CLI   QMEDIA,C'N'                                                      
         BNE   RAT12                                                            
         CLI   PUBRSPCE,C' '       SKIP SPACE RATES                             
         BH    RAT4                                                             
         MVI   INFSW,1                                                          
*        GOTO1 DTCNV,DMCB,(1,PUBRSTRT),(3,RTDAT)                                
         GOTO1 DATCON,DMCB,(3,PUBRSTRT),(5,RTDAT)                               
*                                                                               
         EDIT  (P5,PUBRATE),(10,RTRAT),5,ALIGN=LEFT                             
*                                                                               
         LA    R6,RTRAT-3                                                       
         AR    R6,R0                                                            
         CLC   =C'000',0(R6)                                                    
         BNE   *+14                                                             
         MVC   0(3,R6),SPACES                                                   
         B     *+8                                                              
*                                                                               
         LA    R6,3(R6)                                                         
         MVI   0(R6),C'/'                                                       
         MVC   1(4,R6),=C'LINE'                                                 
         CLI   PUBRTYP,X'80'                                                    
         BE    RAT13                                                            
         CLI   PUBRTYP,0                                                        
         BE    RAT13                                                            
         MVC   1(4,R6),=C'INCH'                                                 
         CLI   PUBRTYP,X'20'                                                    
         BE    RAT13                                                            
         MVC   1(10,R6),=C'AGATE LINE'                                          
         B     RAT13                                                            
*                                                                               
RAT12    DS    0H                                                               
         CLI   PUBRSPCE,C' '       SKIP NON-SPACE RATES                         
         BNH   RAT4                                                             
*                                                                               
         CLC   PUBRSPCE(2),=C'R='           IF ITS R= RATE CODE                 
         BE    PRTCDS                       GOTO PRINT RATE CODES.              
         CLI   QOPT1,C'K'                   IF NOT R=, & QOPT=K SKIP            
         BE    RAT4                         THIS ELEMENT.                       
*                                                                               
         MVI   INFSW,1                                                          
         EDIT  (P5,PUBRATE),(10,P+94),2                                         
         LA    R6,PBYOWK                                                        
         USING PPBYOUTD,R6                                                      
*                                                                               
         LA    RF,PUBRSPCE                                                      
         ST    RF,PBYOINPT                                                      
         MVI   PBYOCTL,X'C0'       SPACE DESC INPUT                             
         GOTO1 PPBYOUT,DMCB,PPBYOUTD                                            
         MVC   P+72(20),PBYOSPC1                                                
*                                                                               
*        GOTO1 DTCNV,(R1),(1,PUBRSTRT),(3,P+107)                                
         GOTO1 DATCON,(R1),(3,PUBRSTRT),(5,P+107)                               
         DROP  R6                                                               
         B     RAT16                                                            
*                                                                               
PRTCDS   CLI   QOPT1,C'S'                        IF ITS R=RATECODE, &           
         BE    RAT4                              QOPT1=S, SKIP ELEMENT.         
         MVI   INFSW,1                                                          
         MVI   RCSUBPRG,17                MOVE IN SPROG FOR RATE CODES.         
         CLI   PUBRTYP,0                         IS TYPE = TOTAL.               
         BE    PRTCDS5                                                          
         EDIT  (P5,PUBRATE),(10,P+94),5       LINES, INCHES.                    
         B     PRTCDS6                                                          
PRTCDS5  EDIT  (P5,PUBRATE),(10,P+94),2       TOTAL.                            
PRTCDS6  MVC   P+72(3),PUBRSPCE+2             MOVE TO PRINT LINE, CODE.         
         MVC   P+78(12),PUBRSPCE+5                               DESCR.         
RT35     TM    PUBRTYP,X'80'                  IF TYPE = LINES, MOVE             
         BNO   RT35A                          '/L' AFTER RATE.                  
         MVI   P+105,C'L'                                                       
         B     RT35B                                                            
RT35A    TM    PUBRTYP,X'20'                  IF TYPE = INCHES, MOVE            
         BNO   RT35C                          'I/' AFTER INCHES.                
         MVI   P+105,C'I'                                                       
RT35B    MVI   P+104,C'/'                                                       
*RT35C    GOTO1 DTCNV,(R1),(1,PUBRSTRT),(3,P+109)   DATE TO PRINT LINE.         
RT35C    GOTO1 DATCON,(R1),(3,PUBRSTRT),(5,P+109)  DATE TO PRINT LINE.          
         B     RAT16                                                            
*                                                                               
RAT13    DS    0H                                                               
*                                  LOOK TO NEXT ELEM                            
         SR    R6,R6                                                            
         IC    R6,1(R4)                                                         
         AR    R6,R4                                                            
         CLI   0(R6),X'51'                                                      
         BNE   RAT16                                                            
         LA    R1,RTDLEV                                                        
         MVC   0(8,R1),=C'   LINES'                                             
         CLI   PUBDLTYP,C'L'                                                    
         BE    RAT14                                                            
         MVC   0(8,R1),=C'   PAGES'                                             
         CLI   PUBDLTYP,C'P'                                                    
         BE    RAT14                                                            
         MVC   0(8,R1),=C'   TIMES'                                             
         CLI   PUBDLTYP,C'X'                                                    
         BE    RAT14                                                            
         MVC   0(8,R1),=C' DOLLARS'                                             
         CLI   PUBDLTYP,C'$'                                                    
         BE    RAT14                                                            
         MVC   0(8,R1),=C'  INCHES'                                             
         CLI   PUBDLTYP,C'I'                                                    
         BE    RAT14                                                            
         MVC   0(8,R1),SPACES                                                   
*                                                                               
RAT14    DS    0H                                                               
         LA    R1,RTDRAT                                                        
         MVC   0(4,R1),=C'RATE'                                                 
         CLI   PUBDRTYP,X'80'                                                   
         BE    RAT16                                                            
         MVC   0(4,R1),=C'PCT '                                                 
         CLI   PUBDRTYP,X'40'                                                   
         BE    RAT16                                                            
         MVC   0(4,R1),SPACES                                                   
*                                                                               
RAT16    DS    0H                                                               
         CLC   P+18(20),SPACES     NAME                                         
         BNE   RAT16B                                                           
*                                                                               
         CLI   PUBLANG,C'F'         SEE IF FRENCH                               
         BNE   *+10                                                             
         MVC   P(6),=C'FRENCH'                                                  
*                                                                               
         MVC   P+18(20),PUBZNAME                                                
         MVC   PUBZNAME,SPACES                                                  
RAT16B   DS    0H                                                               
         BAS   RE,PRINTITR                                                      
         B     RAT4                                                             
*                                                                               
*                                  RATE DISCOUNT ELEM                           
RAT20    DS    0H                                                               
         B     RAT4                NOOP DISCOUNT RATES                          
         USING PUBDSCEL,R4                                                      
         EDIT  (P5,PUBDSCLV),(9,RTDLEV),COMMAS=YES                              
*                                                                               
         EDIT  (P5,PUBDSCRT),(10,RTDRAT),5,ALIGN=LEFT                           
*                                                                               
         LA    R6,RTDRAT-3                                                      
         AR    R6,R0                                                            
         CLC   =C'000',0(R6)                                                    
         BNE   *+10                                                             
         MVC   0(3,R6),SPACES                                                   
*                                                                               
         B     RAT16                                                            
*                                                                               
RAT30    DS    0H                                                               
         CLC   P,SPACES            TEST ANYTHING PRINTED                        
         BNE   RAT32               NO                                           
         CLC   PUBZNAME,SPACES                                                  
         BE    RAT32                                                            
*                                                                               
         CLI   PUBLANG,C'F'         SEE IF FRENCH                               
         BNE   *+10                                                             
         MVC   P(6),=C'FRENCH'                                                  
*                                                                               
         MVC   P+18(20),PUBZNAME                                                
         BAS   RE,PRINTITR                                                      
         B     RAT32                                                            
*                                                                               
RAT32    DS    0H                                                               
         CLI   QOPT1,C'S'          IF SAUS                                      
         BE    RAT38                                                            
         CLI   QOPT1,C'K'                                                       
         BNE   RAT40                                                            
*                                                                               
RAT38    MVC   QMEDIA,MYSVMED      RESTORE REQUESTED MEDIA                      
*                                                                               
RAT40    CLI   INFSW,1             SEE IF RATE INFO PRINTED                     
         BNE   TAXRT                                                            
*****          THIS IS DSPCE ROUTINE WITHOUT EXITING PROGRAM                    
         CLI   QOPT3,C'Y'          SEE IF DOUBLE SPACING                        
         BNE   TAXRT                                                            
         SR    RE,RE                                                            
         IC    RE,LINE                                                          
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
         CLC   BYTE,MAXLINES                                                    
         BH    TAXRT                                                            
         GOTO1 REPORT                                                           
         EJECT                                                                  
*                                                                               
TAXRT    DS    0H                                                               
         CLI   PUBGST,0                                                         
         BE    TAXRTA                                             L02           
         LA    R5,P+63                                            L02           
         CLI   QMEDIA,C'M'                                        L02           
         BNE   *+8                                                L02           
         LA    R5,9(R5)                                           L02           
         MVC   0(14,R5),=C'*GST TAX CODE='                        L02           
         MVC   14(1,R5),PUBGST                                    L02           
         BAS   RE,PRINTIT                                                       
         MVI   INFSW,1                                                          
*                                                                               
TAXRTA   DS    0H                                                               
*                                                                               
*        DISPLAY PST CODES                                                      
         XC    PSTOUT(64),PSTOUT                                                
*                                                                               
         LA    R2,PUBREC+33                                                     
PRTPST2  CLI   0(R2),X'90'                                                      
         BE    PRTPST5                                                          
         CLI   0(R2),0             END OF REC                                   
         BE    TAXRTC                                                           
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     PRTPST2                                                          
*                                                                               
PRTPST5  LA    R1,2(R2)             POINT R1 TO PSTCODES                        
         LA    R2,PSTBLK                                                        
         USING PSTBLKD,R2                                                       
         XC    0(PSTLNQ,R2),0(R2)     CLEAR INTERFACE BLOCK                     
         MVI   PSTACT,PSTFMTQ      ACTION = FORMAT                              
         ST    R1,PSTADIN          INPUT ADDRESS                                
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,VCOMFACS    A(COMFACS)                                   
         GOTO1 APSTVAL,DMCB,(R2)                                                
         CLI   PSTOUT,C' '       SEE IF I HAVE PST CODES                        
         BNH   TAXRTC                                                           
         LA    R5,P+63                                            L02           
         CLI   QMEDIA,C'M'                                        L02           
         BNE   *+8                                                L02           
         LA    R5,9(R5)                                           L02           
         MVC   0(4,R5),=C'*PST'                                                 
         MVC   5(49,R5),PSTOUT                                                  
         DROP  R2                                                               
         GOTO1 PRINTIT                                                          
         MVI   INFSW,1                                                          
*                                                                               
TAXRTC   LA    R4,PUBREC+33                                                     
TAXR5    CLI   0(R4),0                                                          
*****    BE    PREXT                                                            
         BE    TAXREND                                                          
         CLI   0(R4),X'22'            FIND '22' (TAX) ELEMENT                   
         BE    TAXR10                                                           
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     TAXR5                                                            
TAXR10   DS    0H                                                               
         LA    R4,2(R4)               BUMP UP R4 TO POINT AT FIRST RATE         
         USING TAXELD,R4              DSECT TO COVER 1 RATE AND 1 DATE          
         LA    R6,3                   DO MAX OF THREE TIMES                     
TAXR20   CLI   ELDTE,0                                                          
*****    BE    PREXT                                                            
         BE    TAXREND                                                          
TAXR25   MVC   PSECOND,SPACES                                                   
         LA    R5,P                   ADDRESS PRINT AREA                        
         USING TAXLIND,R5                                                       
         CLI   QMEDIA,C'M'            IF MEDIA IS MAG PUT OUT RATES             
         BNE   TAXR30                 FURTHER TO THE RIGHT                      
         LA    R5,9(R5)                                                         
TAXR30   MVC   RATEQ,=C'TAX RATE= '                                             
         EDIT  ELRTE,(7,TAXRTE),4,ALIGN=LEFT        TAX RATE                    
         CLI   QMEDIA,C'M'                                                      
         BNE   TAXR35                                                           
         LA    R5,4(R5)                                                         
TAXR35   MVC   DATEQ,=C'EFFECTIVE DATE= '                                       
*        GOTO1 DTCNV,DMCB,(1,ELDTE),(3,TAXDTE)      EFFECTIVE DATE              
         GOTO1 DATCON,DMCB,(3,ELDTE),(5,TAXDTE)     EFFECTIVE DATE              
TAXR50   BAS   RE,PRINTITR                          PRINT IT                    
         MVI   INFSW,1                                                          
         LA    R4,6(R4)             BUMP TO NEXT RATE AND DATE                  
         BCT   R6,TAXR20                                                        
*****    B     PREXT                                                            
*                                                                               
TAXREND  CLI   INFSW,1             ANY ACTIVITY PRINTED ?                       
         BE    PREXT               YES                                          
         SP    PUBCNT,=P'1'        NO - ADJUST PUB COUNT                        
         B     PREXT                                                            
         EJECT                                                                  
*                                  PREMIUMS                                     
PREM     DS    0H                                                               
         MVI   INFSW,0                                                          
         MVI   RCSUBPRG,8                                                       
         MVC   PSECOND,SPACES                                                   
         LA    R5,P                                                             
         USING PRLIND,R5                                                        
         LA    R4,PUBREC+33                                                     
PRM2     DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    PRM30                                                            
         CLI   0(R4),X'60'                                                      
         BE    PRM10                                                            
         CLI   0(R4),X'61'                                                      
         BE    PRM20                                                            
PRM4     DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     PRM2                                                             
*                                  PREM DESC ELEM                               
PRM10    DS    0H                                                               
         MVI   INFSW,1                                                          
         USING PUBPRMEL,R4                                                      
         MVC   PRTYP,PUBPCOD                                                    
         MVC   PRCLT,=C'ALL'                                                    
         CLI   PUBPCLT,C' '                                                     
         BNH   PRM11                                                            
         MVC   PRCLT,PUBPCLT                                                    
PRM11    GOTO1 DATCON,DMCB,(3,PUBPSTRT),(5,PRDAT)                               
*                                                                               
         MVC   PRMINS,PUBPMINS                                                  
         CLC   PUBPMINS,=C'PAGE'                                                
         BE    PRM12                                                            
         EDIT  (P4,PUBPMINS),(4,PRMINS)                                         
*                                                                               
PRM12    DS    0H                                                               
         CP    PUBPMINC,=P'0'                                                   
         BE    PRM14                                                            
         EDIT  (P5,PUBPMINC),(8,PRMINC),2,FLOAT=$                               
*                                                                               
PRM14    DS    0H                                                               
         MVC   METH,PUBPTYPC                                                    
         MVC   PRTYPC,=C'  FLAT '                                               
         CLI   PUBPTYPC,X'80'                                                   
         BE    PRM16                                                            
         MVC   PRTYPC,=C'BW+FLAT'                                               
         CLI   PUBPTYPC,X'40'                                                   
         BE    PRM16                                                            
         MVC   PRTYPC,=C'BW+PCT.'                                               
         CLI   PUBPTYPC,X'20'                                                   
         BE    PRM16                                                            
         MVC   PRTYPC,=C'BW+LINE'                                               
         CLI   PUBPTYPC,X'10'                                                   
         BE    PRM16                                                            
         MVC   PRTYPC,=C'BW+INCH'                                               
         CLI   PUBPTYPC,X'08'                                                   
         BE    PRM16                                                            
         MVC   PRTYPC,SPACES                                                    
*                                                                               
PRM16    DS    0H                                                               
         B     PRM4                                                             
*                                  PREM TABLE ELEM                              
PRM20    DS    0H                                                               
         USING PUBPTBEL,R4                                                      
         CLI   METH,X'80'          FLAT ?                                       
         BE    PRM20D              YES                                          
         CLI   METH,X'40'          BW+FLAT ?                                    
         BE    PRM20D              YES                                          
*        EDIT  (P5,PUBPTBCH),(9,PRCHARGE),2                                     
         EDIT  (P6,PUBPTBCH),(10,PRCHARGE),5,ALIGN=RIGHT,DROP=3                 
         B     PRM21                                                            
PRM20D   DS    0H                                                               
         XC    DUB,DUB                                                          
         MVC   DUB+2(6),PUBPTBCH                                                
         DP    DUB,=P'1000'                                                     
         EDIT  (P5,DUB),(10,PRCHARGE),2,ALIGN=LEFT                              
*RM20D   EDIT  (P5,PUBPTBCH),(9,PRCHARGE),2,FLOAT=$                             
*                                                                               
PRM21    DS    0H                                                               
         MVC   PRLINES,=C' UNLIMITED'                                           
         CP    PUBPTBLN,=P'999999999'                                           
         BE    PRM22                                                            
         MVC   PRLINES,=C'SPOT COLOR'                                           
         CP    PUBPTBLN,=P'0'                                                   
         BE    PRM22                                                            
         EDIT  (P5,PUBPTBLN),(10,PRLINES)                                       
*NOP*    EDIT  (P5,PUBPTBLN),(10,PRLINES),ALIGN=LEFT                            
*                                                                               
PRM22    DS    0H                                                               
         CLC   P+18(20),SPACES     NAME                                         
         BNE   PRM22B                                                           
*                                                                               
         CLI   PUBLANG,C'F'         SEE IF FRENCH                               
         BNE   *+10                                                             
         MVC   P(6),=C'FRENCH'                                                  
*                                                                               
         MVC   P+18(20),PUBZNAME                                                
         MVC   PUBZNAME,SPACES                                                  
PRM22B   DS    0H                                                               
         BAS   RE,PRINTITR                                                      
         B     PRM4                                                             
*                                                                               
*                                                                               
PRM30    DS    0H                                                               
*                                  PRINT NOTHING IF NO PREMS                    
         CLC   P,SPACES                                                         
         BNE   PRMX                                                             
         CLC   PUBZNAME,SPACES     CHECK ZONE NAME TO PRINT                     
         BE    PRMX                                                             
*                                                                               
         CLI   PUBLANG,C'F'         SEE IF FRENCH                               
         BNE   *+10                                                             
         MVC   P(6),=C'FRENCH'                                                  
*                                                                               
         MVC   P+18(20),PUBZNAME                                                
         BAS   RE,PRINTITR                                                      
PRMX     CLI   INFSW,1             SEE IF PREM INFO PRINTED                     
         BE    DSPCE               YES MAY NEED TO DOUBLE SPACE                 
         SP    PUBCNT,=P'1'        ADJUST PUB COUNT                             
         B     PREXT               NO THEN PRINT NOTHING                        
         EJECT                                                                  
*                   CLE - CONTRACT LINEAGE EQUIV.                               
         SPACE 2                                                                
CLE      DS    0H                                                               
         MVI   INFSW,0                                                          
         MVI   RCSUBPRG,15                                                      
         MVC   PSECOND,SPACES                                                   
         LA    R5,P                                                             
         USING CLLIND,R5                                                        
*                                                                               
         LA    R4,PUBREC+33                                                     
CLE2     DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    CLE30                                                            
         CLI   0(R4),X'40'                                                      
         BE    CLE10                                                            
CLE4     DS    0H                                                               
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     CLE2                                                             
*                                                                               
CLE10    DS    0H                                                               
         MVI   INFSW,1                                                          
         USING PUBCLELM,R4                                                      
         MVC   CLSPACE,PUBCLSPC                                                 
         EDIT  (B2,PUBCLLNS),(6,CLLINES)                                        
*                                                                               
CLE16    DS    0H                                                               
         CLC   P+18(20),SPACES                                                  
         BNE   CLE16B                                                           
*                                                                               
         CLI   PUBLANG,C'F'         SEE IF FRENCH                               
         BNE   *+10                                                             
         MVC   P(6),=C'FRENCH'                                                  
*                                                                               
         MVC   P+18(20),PUBZNAME                                                
         MVC   PUBZNAME,SPACES                                                  
*                                                                               
CLE16B   DS    0H                                                               
         BAS   RE,PRINTITR                                                      
         B     CLE4                                                             
*                                                                               
CLE30    DS    0H                                                               
         CLC   P,SPACES            TEST ANYTHING PRINTED                        
         BNE   CLEX                NO                                           
         CLC   PUBZNAME,SPACES                                                  
         BE    CLEX                                                             
*                                                                               
         CLI   PUBLANG,C'F'         SEE IF FRENCH                               
         BNE   *+10                                                             
         MVC   P(6),=C'FRENCH'                                                  
*                                                                               
         MVC   P+18(20),PUBZNAME                                                
         BAS   RE,PRINTITR                                                      
CLEX     CLI   INFSW,1             SEE IF CLE INFO PRINTED                      
         BE    DSPCE               YES - MAY NEED TO DOUBLE SPACE               
         SP    PUBCNT,=P'1'        ADJUST PUB COUNT                             
         B     PREXT               NO - PRINT NOTHING                           
         EJECT                                                                  
*                                                                               
ADV      DS    0H                                                               
         MVI   INFSW,0                                                          
         MVI   RCSUBPRG,21                                                      
         MVC   P+64(18),=C'*** UNASSIGNED ***'                                  
         LA    R4,PUBREC+33                                                     
         MVI   ELCODE1,X'80'                                                    
ADV5     BAS   RE,NEXTEL                                                        
         BNE   ADVX                                                             
         USING PUBADVD,R4                                                       
         CLC   QCLIENT,=C'ALL'          SEE IF DOING ALL ADVERTISERS            
         BE    ADV10                                                            
         CLC   QCLIENT,=C'   '                                                  
         BE    ADV10                                                            
         CLC   PUBADVCD,QCLIENT          OR JUST ONE                            
         BNE   ADV5                                                             
*                                                                               
ADV10    MVC   P+64(19),SPACES                                                  
         MVC   P+64(3),PUBADVCD                                                 
         ZIC   R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),PUBADVPC),(C'S',P+69)                         
         BAS   RE,PRINTITR                                                      
         MVI   INFSW,1                                                          
         B     ADV5                                                             
*                                                                               
ADVX     CLI   QOPT6,C'Y'       SEE IF INCLUDING UNASSGINED                     
         BE    ADVXX                                                            
         CLI   INFSW,1         DID I FIND AN ASSIGNMENT?                        
         BE    ADVXX                                                            
         MVC   P,SPACES                                                         
         SP    PUBCNT,=P'1'     ADJUST PUB COUNT                                
         B     PREXT            SKIP                                            
*                                                                               
ADVXX    BAS   RE,PRINTITR                                                      
         B     PREXT                                                            
*                                                                               
         EJECT                                                                  
         SPACE 2                                                                
REP0     DS    0H                                                               
         MVI   REPACT,0                                                         
*                                                                               
         CLI   PUBLANG,C'F'         SEE IF FRENCH                               
         BNE   *+10                                                             
         MVC   PSECOND(6),=C'FRENCH'                                            
*                                                                               
         OC    PUBZNAME,SPACES                                                  
         MVC   PSECOND+18(20),PUBZNAME                                          
         MVI   RCSUBPRG,10                                                      
         CLI   QOPT1,C'M'           PUB/CLT STANDARD COMMENT LISTING            
         BNE   *+8                                                              
         MVI   RCSUBPRG,20                                                      
         MVC   P+63(30),PUBLINE1                                                
         MVC   PSECOND+63(30),PUBLINE2                                          
         MVC   ATTN,SPACES                                                      
         MVC   TELN,SPACES                                                      
         MVC   FAXN,SPACES                                                      
         MVC   TOFAX,SPACES                                                     
*                                                                               
         LA    R4,PUBREC+33                                                     
REP00    DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    REP1                                                             
         CLI   0(R4),X'11'         SUPPL ADDR ELEM                              
         BE    REP01                                                            
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     REP00                                                            
REP01    DS    0H                                                               
         USING PUBSADEL,R4                                                      
         CLI   PUBATTN,C' '                                                     
         BNH   REP12                                                            
         MVC   ATTN(5),=C'ATTN-'                                                
*****    MVC   ATTN+6(24),PUBATTN                                               
         MVC   ATTN+5(24),PUBATTN                                               
REP12    CLI   PUBTEL,C' '                                                      
         BNH   REP13                                                            
         MVC   TELN(2),=C'T='                                                   
         MVC   TELN+2(12),PUBTEL                                                
REP13    CLI   1(R4),60            NEW LENGTH?                                  
         BNH   REP1                                                             
         CLI   PUBSFAXN,C' '                                                    
         BNH   REP1                                                             
         MVC   FAXN(2),=C'F='                                                   
         MVC   FAXN+2(12),PUBSFAXN                                              
         CLC   =C'FX=',PUBSFAXN    NUMBER IN CONTROL FILE ?                     
         BNE   REP1                NO                                           
         MVC   FRSVFAX,PUBSFAXN                                                 
         OC    FRSVFAX,SPACES                                                   
         GOTO1 AGETFAX             GET FAX FROM CONTROL FILE                    
*                                                                               
REP1     DS    0H                                                               
         CLI   QOPT1,C'R'          SHOW PUBLISHER ON REP LISTING                
         BNE   REP1X                                                            
         OC    PUBPLSH,PUBPLSH                                                  
         BZ    REP1X                                                            
         MVI   REPACT,1                                                         
         LA    R6,P+94                                                          
         CLC   PSECOND,SPACES                                                   
         BE    *+8                                                              
         LA    R6,132(R6)                                                       
         MVC   0(10,R6),=C'PUBLISHER='                                          
         MVC   10(4,R6),PUBPLSH                                                 
         XC    PBLNAME,PBLNAME                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(2),PUBKAGY                                                   
         MVC   KEY+2(1),PUBKMED                                                 
         MVI   KEY+3,X'11'                                                      
         MVC   KEY+4(4),PUBPLSH                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    REP1C                                                            
         MVC   PBLNAME(27),=C'** NOT ON FILE - CALL DDS**'                      
         B     REP1E                                                            
*                                                                               
REP1C    MVC   FULL,AREC                                                        
         LA    R0,PREPREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         MVC   PBLNAME,PREPNAME                                                 
         MVC   AREC,FULL       RESTORE PPG'S AREC                               
REP1E    CLI   PSECOND+94,C' '                                                  
         BNH   REP1G                                                            
         BAS   RE,PRINTITR                                                      
         MVC   P+94(30),PBLNAME                                                 
         ST    RF,SAVERF       USED TO BUMP THRU P                              
         LA    RF,P                                                             
         CLC   ATTN,SPACES                                                      
         BE    REP1F1                                                           
         MVC   63(30,RF),ATTN                                                   
         LA    RF,132(RF)                                                       
         MVC   ATTN,SPACES                                                      
REP1F1   CLC   TELN,SPACES                                                      
         BE    REP1F2                                                           
         MVC   63(15,RF),TELN                                                   
         MVC   TELN,SPACES                                                      
REP1F2   CLC   FAXN,SPACES                                                      
         BE    REP1F                                                            
         CLC   63(15,RF),SPACES                                                 
         BNE   REP1F4                                                           
         MVC   63(15,RF),FAXN                                                   
         B     REP1F                                                            
REP1F4   MVC   78(15,RF),FAXN                                                   
REP1F    MVC   FAXN,SPACES                                                      
         L     RF,SAVERF                                                        
         BAS   RE,PRINTITR                                                      
         CLC   TOFAX,SPACES        FAX FROM CONTROL FILE ?                      
         BE    REP1X               NO                                           
         MVC   P+63(7),=C'FAX NO='                                              
         MVC   P+70(16),TOFAX                                                   
         BAS   RE,PRINTITR                                                      
         MVC   TOFAX,SPACES                                                     
         B     REP1X                                                            
*                                                                               
REP1G    MVC   PSECOND+94(30),PBLNAME                                           
         BAS   RE,PRINTITR                                                      
*                                                                               
REP1X    DS    0H                                                               
         LA    R4,PUBREC+33                                                     
REP2     DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    REP30                                                            
         CLI   0(R4),X'14'                                                      
         BE    REP10                                                            
REP4     DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     REP2                                                             
REP10    DS    0H                                                               
         USING PUBREPEL,R4                                                      
         CLI   FILTCLT,0                                                        
         BE    REP11                                                            
         CLC   FILTCLT,PUBRPOFF                                                 
         BNE   REP4                                                             
REP11    DS    0H                                                               
         LA    R6,P+94                                                          
         CLC   PSECOND,SPACES                                                   
         BE    *+8                                                              
         LA    R6,132(R6)                                                       
         CLI   QOPT1,C'M'          SEE IF DOING PUB/CLT STANDARD COM            
         BNE   REP11R                                                           
         MVI   RCSUBPRG,20                                                      
         CLI   PUBREPEL+1,X'2D'      MUST BE BIG ELEM                           
         BL    REP4                  STILL SHOW CLIENT                          
         OC    PUBCSCC1(14),PUBCSCC1    CHK FOR INFO                            
         BZ    REP4                                                             
         MVC   0(6,R6),PUBCSC1                                                  
         CLI   PUBCSCC1,0                                                       
         BE    REP11F                                                           
         TM    PUBCSCC1,X'80'         INSERTION ORDERS                          
         BNO   *+8                                                              
         MVI   7(R6),C'I'                                                       
         TM    PUBCSCC1,X'40'         CONTRACTS                                 
         BNO   *+8                                                              
         MVI   8(R6),C'C'                                                       
REP11F   MVC   11(6,R6),PUBCSC2                                                 
         CLI   PUBCSCC2,0                                                       
         BE    REP11H                                                           
         TM    PUBCSCC2,X'80'         INSERTION ORDERS                          
         BNO   *+8                                                              
         MVI   18(R6),C'I'                                                      
         TM    PUBCSCC2,X'40'         CONTRACTS                                 
         BNO   *+8                                                              
         MVI   19(R6),C'C'                                                      
*                                                                               
REP11H   DS    0H                                                               
         MVI   REPACT,1                                                         
         CLC   PUBRPOFF,=3X'FF'                                                 
         BE    REP22                                                            
         MVC   22(3,R6),PUBRPOFF                                                
         CLI   PUBRPOFF,X'FF'                                                   
         BNE   REP22                                                            
         MVI   22(R6),C'*'                                                      
*                                                                               
*        PRINT OFFICE CODE                                                      
*                                                                               
         GOTOR VPRNTOFC,DMCB,PUBRPOFF+1,23(R6),VOFFICER,QAGENCY,       X        
               VCOMFACS                                                         
*                                                                               
         B     REP22                                                            
*                                                                               
** REP DISPLAY QOPT1=R                                                          
REP11R   OC    PUBPAREP(24),PUBPAREP                                            
         BZ    REP4                                                             
         OC    PUBPAREP(12),=12C'0'                                             
         CLC   PUBPAREP(12),=12C'0'                                             
         BE    REP18                                                            
         MVC   00(4,R6),=C'NONE'                                                
         CLC   PUBPAREP,=4C'0'                                                  
         BE    *+10                                                             
         MVC   00(4,R6),PUBPAREP                                                
         MVC   06(4,R6),=C'NONE'                                                
         CLC   PUBTRREP,=4C'0'                                                  
         BE    *+10                                                             
         MVC   06(4,R6),PUBTRREP                                                
         MVC   12(4,R6),=C'NONE'                                                
         CLC   PUBCNREP,=4C'0'                                                  
         BE    *+10                                                             
         MVC   12(4,R6),PUBCNREP                                                
*                                                                               
REP18    DS    0H                                                               
         MVI   REPACT,1                                                         
         CLC   PUBRPOFF,=3X'FF'                                                 
         BE    REP20                                                            
         MVC   18(3,R6),PUBRPOFF                                                
         CLI   PUBRPOFF,X'FF'                                                   
         BNE   REP20                                                            
         MVI   18(R6),C'*'                                                      
*                                                                               
*        PRINT OFFICE CODE                                                      
*                                                                               
         GOTOR VPRNTOFC,DMCB,PUBRPOFF+1,19(R6),VOFFICER,QAGENCY,       X        
               VCOMFACS                                                         
*                                                                               
REP20    DS    0H                                                               
         OC    PUBCVEN,PUBCVEN     CLIENT/VENDOR NO.                            
         BZ    REP22                                                            
         MVI   REPACT,1                                                         
         MVC   23(12,R6),PUBCVEN                                                
         CLI   PUBCVEN,X'99'                                                    
         BNL   REP22                                                            
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),PUBCVEN),(C'S',23(R6))                        
*                                                                               
REP22    DS    0H                                                               
         CLC   P+63(30),SPACES                                                  
         BNE   REP22BX                                                          
         ST    RF,SAVERF                                                        
         LA    RF,P                                                             
         CLC   ATTN,SPACES                                                      
         BE    REP22B1                                                          
         MVC   63(30,RF),ATTN                                                   
         LA    RF,132(RF)                                                       
         MVC   ATTN,SPACES                                                      
REP22B1  CLC   TELN,SPACES                                                      
         BE    REP22B2                                                          
         MVC   63(15,RF),TELN                                                   
         MVC   TELN,SPACES                                                      
REP22B2  CLC   FAXN,SPACES                                                      
         BE    REP22B                                                           
         CLC   63(15,RF),SPACES    SPACE OCCUPIED ?                             
         BNE   REP22B4             YES                                          
         MVC   63(15,RF),FAXN                                                   
         B     REP22B                                                           
REP22B4  MVC   78(15,RF),FAXN                                                   
REP22B   MVC   FAXN,SPACES                                                      
         L     RF,SAVERF                                                        
         BAS   RE,PRINTITR                                                      
         CLC   TOFAX,SPACES        FAX FROM CONTROL FILE ?                      
         BE    REP4                NO                                           
         MVC   P+63(7),=C'FAX NO='                                              
         MVC   P+70(16),TOFAX                                                   
         MVC   TOFAX,SPACES                                                     
REP22BX  BAS   RE,PRINTITR                                                      
         B     REP4                                                             
*                                                                               
REP30    DS    0H                                                               
         CLI   REPACT,1               CHECK FOR REP ACTIVITY                    
         BE    REP30C                 NO - PRINT NOTHING                        
         SP    PUBCNT,=P'1'        ADJUST PUB COUNT                             
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         B     PREXT                                                            
*                                                                               
REP30C   CLC   P,SPACES                                                         
         BNH   REP31                                                            
         BAS   RE,PRINTITR                                                      
REP31    DS    0H                                                               
*****    ST    RF,SAVERF                                                        
*****    LA    RF,P                                                             
         CLC   ATTN,SPACES                                                      
         BNH   REPX1                                                            
*****    MVC   63(30,RF),ATTN                                                   
         MVC   P+63(30),ATTN                                                    
*****    LA    RF,132(RF)                                                       
         BAS   RE,PRINTITR                                                      
REPX1    CLC   TELN,SPACES                                                      
         BNH   REPX2                                                            
*****    MVC   63(20,RF),TELN                                                   
         MVC   P+63(20),TELN                                                    
         BAS   RE,PRINTITR                                                      
REPX2    CLC   FAXN,SPACES                                                      
         BNH   REPXX                                                            
         MVC   P+63(20),FAXN                                                    
         BAS   RE,PRINTITR                                                      
         CLC   TOFAX,SPACES        FAX FROM CONTROL FILE ?                      
         BE    REPXX               NO                                           
         MVC   P+63(7),=C'FAX NO='                                              
         MVC   P+70(16),TOFAX                                                   
         BAS   RE,PRINTITR                                                      
         MVC   TOFAX,SPACES                                                     
*                                                                               
REPXX    DS    0H                                                               
         B     DSPCE                                                            
*                                                                               
         EJECT                                                                  
         SPACE 2                                                                
PAY      DS    0H                                                               
         MVI   INFSW,0                                                          
         MVI   REPACT,0                                                         
         ZAP   PAYCNT,=P'1'        FOR "LINE 2" PRINTING                        
         MVC   PSECOND,SPACES                                                   
         MVI   RCSUBPRG,23           FOR HEADLINES                              
         LA    R4,PUBREC+33                                                     
*                                                                               
PAY2     DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    PAYX                                                             
         CLI   0(R4),X'14'                                                      
         BE    PAY6                                                             
PAY4     DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     PAY2                                                             
PAY6     DS    0H                                                               
         USING PUBREPEL,R4                                                      
         CLI   FILTCLT,0                                                        
         BE    PAY8                                                             
         CLC   FILTCLT,PUBRPOFF                                                 
         BNE   PAY4                                                             
PAY8     DS    0H                                                               
         CLI   PUBREPEL+1,X'2D'      MUST BE BIG ELEM                           
         BL    PAY4                  CHK FOR ANOTHER '14' ELEMENT               
*                                                                               
PAY10    DS    0H                                                               
         MVC   P+63(3),=C'ALL'                                                  
         CLC   PUBRPOFF,=3X'FF'                                                 
         BE    PAY10C                                                           
         MVC   P+63(3),PUBRPOFF                                                 
         CLI   PUBRPOFF,X'FF'                                                   
         BNE   PAY10C                                                           
         MVI   P+63,C'*'                                                        
*                                                                               
*        PRINT OFFICE CODE                                                      
*                                                                               
         GOTOR VPRNTOFC,DMCB,PUBRPOFF+1,P+64,VOFFICER,QAGENCY,VCOMFACS          
*                                                                               
PAY10C   TM    PUBCCTL,X'01'                                                    
         BNO   PAY10D                                                           
         MVI   REPACT,1                                                         
         MVI   INFSW,1                                                          
         MVC   P+70(3),=C'YES'                                                  
*                                                                               
PAY10D   CLI   PUBREPEL+1,X'35'    ELEMENT LNTH >= 53 ?                         
         BL    PAY30               NO PAY CONTROL FIELDS IN ELEMENT             
         CLI   PUBPCTL1,C'Y'                                                    
         BNE   PAY10E                                                           
         MVI   REPACT,1                                                         
         MVI   INFSW,1                                                          
         MVC   P+80(3),=C'YES'                                                  
*                                                                               
PAY10E   CLI   PUBPCTL2,C'Y'                                                    
         BE    PAY10E2                                                          
         CLI   PUBPCTL2,C'O'                                                    
         BNE   PAY10G                                                           
PAY10E2  MVI   REPACT,1                                                         
         MVI   INFSW,1                                                          
         MVC   P+91(1),PUBPCTL2                                                 
*                                                                               
PAY10G   CLI   PUBPCTL3,C'Y'                                                    
         BE    PAY10G2                                                          
         CLI   PUBPCTL3,C'O'                                                    
         BE    PAY10G2                                                          
         CLI   PUBPCTL3,C'I'                                                    
         BNE   PAY30                                                            
PAY10G2  MVI   REPACT,1                                                         
         MVI   INFSW,1                                                          
         MVC   P+99(1),PUBPCTL3                                                 
*                                                                               
PAY30    DS    0H                                                               
         CLI   REPACT,1              ANY PAY ACTIVITY ?                         
         BNE   PAY4                  NO - CHK FOR ANOTHER '14' ELEMENT          
*                                                                               
PAY30C   CP    PAYCNT,=P'2'        PRINTING LINE 2 ?                            
         BNE   PAY30P              NO                                           
         BAS   RE,PAYTEST          CHECK FOR PUBZNAME                           
PAY30P   BAS   RE,PRINTITR                                                      
         AP    PAYCNT,=P'1'                                                     
         MVI   REPACT,0                                                         
         B     PAY4                  CHK FOR ANOTHER '14' ELEMENT               
*                                                                               
PAYX     DS    0H                                                               
         CP    PAYCNT,=P'2'        PRINTING LINE 2 ?                            
         BNE   PAYXIT              NO                                           
         BAS   RE,PAYTEST          CHECK FOR PUBZNAME                           
         CLC   P,SPACES                                                         
         BE    PAYXIT                                                           
         BAS   RE,PRINTITR                                                      
*                                                                               
PAYXIT   MVC   P,SPACES                                                         
         CLI   INFSW,1                                                          
         BE    PREXT                                                            
         SP    PUBCNT,=P'1'        ADJUST PUB COUNT                             
         B     PREXT                                                            
*                                                                               
*                                                                               
PAYTEST  NTR1                                                                   
         CLI   PUBLANG,C'F'         SEE IF FRENCH                               
         BNE   *+10                                                             
         MVC   P(6),=C'FRENCH'                                                  
         OC    PUBZNAME,SPACES                                                  
         MVC   P+18(20),PUBZNAME                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                  SPECIAL ADDRESS LISTING                      
         SPACE 2                                                                
ADDR     DS    0H                                                               
         MVI   INFSW,0                                                          
         MVI   PPSW,0             CLEAR PUB DATA PRINTED SWITCH                 
*                                                                               
         MVI   QOPT3,C'Y'           ALWAYS DOUBLE SPACE ADDRESSES               
*                                                                               
         XC    PREPREC(197),PREPREC     CLEAR REP RECORD                        
*                                                                               
         CLI   PUBLANG,C'F'         SEE IF FRENCH                               
         BNE   *+10                                                             
         MVC   PSECOND(6),=C'FRENCH'                                            
*                                                                               
         OC    PUBZNAME,SPACES                                                  
         MVC   PSECOND+18(20),PUBZNAME                                          
         MVI   ELCODE1,X'08'                                                    
         MVI   RCSUBPRG,12                                                      
         CLI   QOPT1,C'1'                                                       
         BE    ADDR2                                                            
         MVI   ELCODE1,X'09'                                                    
         MVI   RCSUBPRG,13                                                      
         CLI   QOPT1,C'2'                                                       
         BE    ADDR2                                                            
         MVI   ELCODE1,X'0A'                                                    
         MVI   RCSUBPRG,14                                                      
         CLI   QOPT1,C'3'                                                       
         BE    ADDR2                                                            
         MVI   ELCODE1,X'0B'                                                    
         MVI   RCSUBPRG,22                                                      
         CLI   QOPT1,C'4'            SHIPPING ADDR                              
         BE    ADDR2                                                            
         DC    H'0'                                                             
ADDR2    DS    0H                                                               
         MVC   P+64(30),PUBLINE1                                                
         MVC   PSECOND+64(30),PUBLINE2                                          
         MVC   ATTN,SPACES                                                      
         MVC   TELN,SPACES                                                      
         MVC   FAXN,SPACES                                                      
         MVC   TOFAX,SPACES                                                     
         MVC   SVPATTN,SPACES                                                   
         MVC   SVPTELN,SPACES                                                   
         MVC   SVPFAXN,SPACES                                                   
         MVC   SVPTOFAX,SPACES                                                  
*                                                                               
         LA    R4,PUBREC+33                                                     
ADDR0    DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    ADDR1                                                            
         CLI   0(R4),X'11'         SUPPL ADDR ELEM                              
         BE    ADDR01                                                           
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     ADDR0                                                            
ADDR01   DS    0H                                                               
         USING PUBSADEL,R4                                                      
         CLI   PUBATTN,C' '                                                     
         BNH   ADDR1A                                                           
         MVC   SVPATTN(5),=C'ATTN-'                                             
         MVC   SVPATTN+5(24),PUBATTN                                            
ADDR1A   CLI   PUBTEL,C' '                                                      
         BNH   ADDR1B                                                           
         MVC   SVPTELN(4),=C'TEL-'                                              
         MVC   SVPTELN+4(12),PUBTEL                                             
ADDR1B   CLI   1(R4),60                                                         
         BNH   ADDR1                                                            
         CLI   PUBSFAXN,C' '                                                    
         BNH   ADDR1                                                            
         MVC   SVPFAXN(4),=C'FAX-'                                              
         MVC   SVPFAXN+4(12),PUBSFAXN                                           
         CLC   =C'FX=',PUBSFAXN    NUMBER IN CONTROL FILE ?                     
         BNE   ADDR1               NO                                           
         MVC   FRSVFAX,PUBSFAXN                                                 
         OC    FRSVFAX,SPACES                                                   
         GOTO1 AGETFAX             GET FAX FROM CONTROL FILE                    
         MVC   SVPTOFAX,TOFAX      SAVE PUB'S TOFAX                             
*                                                                               
ADDR1    DS    0H                                                               
         CLI   PROF48,C'Y'       CHECK ALL SOURCES                              
         BE    ADDR50                                                           
*                                                                               
         MVC   FULL,AREC           SAVE PPG'S AREC                              
         MVI   ADDRSW,C' '         RECORD OR ELEMENT ADDRESS SWITCH             
         XC    KEY,KEY                                                          
         MVC   KEY(PUBKCOD-PUBKEY),PUBREC                                       
         MVI   KEY+PUBKCOD-PUBKEY,X'82'      PUB ADDRESS RECORD CODE            
         MVC   KEY+10(1),ELCODE1             ADDRESS TYPE                       
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'PUBDIR',KEY,KEY                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(11),KEYSAVE     MED/PUB(6)/AGY/COD/TYP                       
         BNE   ADDR14          NO ADDRESS RECORD - CHECK FOR ELEMENTS           
         MVI   ADDRSW,C'R'         HAVE ADDRESS RECORD                          
         B     ADDR4                                                            
ADDR3    DS    0H                                                               
         CLI   ADDRSW,C'R'         SEARCHING ADDRESS RECORDS ?                  
         BNE   ADDR14D             NO - CHECK FOR NEXT ELEMENT                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'PUBDIR',KEY,KEY                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
ADDR3C   CLC   KEY(11),KEYSAVE     MED/PUB(6)/AGY/COD/TYP                       
         BNE   ADDR15              DONE                                         
*                                                                               
ADDR4    DS    0H                  GET RECORD TO PRINT ADDRESS                  
*                                                                               
******** GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'PUBFILE',KEY+27,PCONREC,          
********       DMWORK                                                           
*                                                                               
         L     R4,ACONIO1          USES CONTRACT RECORD I/O AREA                
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'PUBFILE',KEY+27,(R4),    +        
               DMWORK                                                           
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
******** LA    R4,PCONREC+33       USE THIS REC AREA FOR I/O                    
         LA    R4,33(R4)           USE THIS REC AREA FOR I/O                    
*                                                                               
ADDR6    DS    0H                                                               
         MVI   LNEED,5                                                          
         USING PUBAOVEL,R4                                                      
*                                                                               
         OC    PUBAOFAX,PUBAOFAX   FAX ?                                        
         BZ    *+8                                                              
         MVI   LNEED,6                                                          
         OC    PUBAOEAD,PUBAOEAD   E-MAIL ?                                     
         BZ    *+8                                                              
         MVI   LNEED,8                                                          
         CLI   FILTCLT,0                                                        
         BE    *+14                                                             
         CLC   FILTCLT,PUBAOFF                                                  
         BNE   ADDR3               NEXT ADDRESS RECORD OR ELEMENT               
         MVI   INFSW,1                                                          
         LA    R5,P+128                                                         
         MVC   0(3,R5),=C'ALL'                                                  
         CLC   PUBAOFF,=3X'FF'                                                  
         BE    ADDR6B                                                           
         MVC   0(3,R5),PUBAOFF                                                  
         CLI   PUBAOFF,X'FF'                                                    
         BNE   ADDR6B                                                           
         MVI   0(R5),C'*'                                                       
*                                                                               
*        PRINT OFFICE CODE                                                      
*                                                                               
         GOTOR VPRNTOFC,DMCB,PUBAOFF+1,1(R5),VOFFICER,QAGENCY,VCOMFACS          
*                                                                               
ADDR6B   DS    0H                                                               
         OC    PUBAONAM(122),SPACES                                             
         LA    R5,P+97                                                          
         MVC   000(30,R5),PUBAONAM                                              
         MVC   132(30,R5),PUBAOLN1                                              
         BAS   RE,PRINTITR                                                      
         MVC   0(30,R5),PUBAOLN2                                                
         BAS   RE,GETDATA                                                       
         MVC   P+64(32),WORK                                                    
         BAS   RE,PRINTITR                                                      
         CLC   PUBAOATN,SPACES                                                  
         BE    ADDR6D                                                           
         MVC   0(5,R5),=C'ATTN-'                                                
         MVC   5(20,R5),PUBAOATN                                                
         BAS   RE,GETDATA                                                       
         MVC   P+64(32),WORK                                                    
         BAS   RE,PRINTITR                                                      
*                                                                               
ADDR6D   DS    0H                                                               
         CLC   PUBAOTEL,SPACES                                                  
         BE    ADDR6F                                                           
         MVC   000(04,R5),=C'TEL-'                                              
         MVC   004(12,R5),PUBAOTEL                                              
         BAS   RE,GETDATA                                                       
         MVC   P+64(32),WORK                                                    
         BAS   RE,PRINTITR                                                      
*                                                                               
ADDR6F   DS    0H                                                               
*                                                                               
         OC    PUBAOFAX,PUBAOFAX   FAX?                                         
         BZ    ADDR8               NO                                           
         MVC   0(04,R5),=C'FAX-'                                                
         MVC   4(12,R5),PUBAOFAX                                                
         CLC   =C'FX=',PUBAOFAX    NUMBER IN CONTROL FILE ?                     
         BNE   ADDR7X              NO - PRINT THE FAX                           
         MVC   FRSVFAX,PUBAOFAX                                                 
         OC    FRSVFAX,SPACES                                                   
         GOTO1 AGETFAX             GET FAX FROM CONTROL FILE                    
         MVI   015(R5),C'('                                                     
         MVC   016(16,R5),TOFAX                                                 
         LA    R1,31(R5)                                                        
ADDR7TA  CLI   0(R1),C' '                                                       
         BH    ADDR7TB                                                          
         BCT   R1,ADDR7TA                                                       
ADDR7TB  MVI   1(R1),C')'                                                       
ADDR7X   BAS   RE,GETDATA          PRINT FAX LINE                               
         MVC   P+64(32),WORK                                                    
         BAS   RE,PRINTITR                                                      
*                                                                               
ADDR8    DS    0H                                                               
*                                                                               
         OC    PUBAOEAD,PUBAOEAD   E-MAIL?                                      
         BZ    ADDR13              NO                                           
*                                                                               
* **  EMAIL ADDRESS TESTING AND MANIPULATION  **                                
* (IF LONGER THAN 25 CHARACTERS MUST USE 2 OR 3 LINES)                          
*                                                                               
         LA    RE,PUBAOEAD         BEGINNING OF E-MAIL ADDRESS                  
         LA    R6,PUBAOEAD+59      END       OF E-MAIL ADDRESS                  
ADDR8B   CLI   0(R6),C' '          BLANK ?                                      
         BH    *+10                NO                                           
         BCTR  R6,0                MOVE "LEFT" 1                                
         B     ADDR8B              TEST NEXT POSITION                           
         SR    R6,RE                                                            
         BNM   *+6                                                              
         DC    H'0'                MUST BE POSITIVE VALUE                       
         LA    R6,1(R6)            ACTUAL LENGTH OF E-MAIL ADDRESS              
*                                                                               
         MVC   0(06,R5),=C'EMAIL-'                                              
         CHI   R6,25               ADDRESS LONGER THAN 25 CHARACTERS ?          
         BH    ADDR8D              YES - USE ECHOP                              
         MVC   6(25,R5),PUBAOEAD   NO - ONE LINE IS ENOUGH                      
         BAS   RE,GETDATA                                                       
         MVC   P+64(32),WORK                                                    
         BAS   RE,PRINTITR                                                      
         B     ADDR13              FINISH ELEMENT                               
*                                                                               
ADDR8D   DS    0H                                                               
*****    MVC   EMAIL1(75),SPACES   CLEAR 3 25-BYTE AREAS                        
*                                                                               
         GOTO1 =V(PPECHOP),DMCB,((R6),PUBAOEAD),(25,EMAIL1),(25,3),0            
         MVC   006(25,R5),EMAIL1   1ST PART OF EMAIL ADDRESS                    
         BAS   RE,GETDATA                                                       
         MVC   P+64(32),WORK                                                    
         BAS   RE,PRINTITR                                                      
         MVC   006(25,R5),EMAIL2   2ND PART OF EMAIL ADDRESS                    
         BAS   RE,GETDATA                                                       
         MVC   P+64(32),WORK                                                    
         BAS   RE,PRINTITR                                                      
         MVC   006(25,R5),EMAIL3   3ND PART OF EMAIL ADDRESS                    
*                                                                               
ADDR13   DS    0H                                                               
         BAS   RE,GETDATA                                                       
         MVC   P+64(32),WORK                                                    
ADDR13P  BAS   RE,PRINTITR                                                      
         BAS   RE,GETDATA     MUST BE SURE I'VE PRINTED EVERYTHING              
         CLC   WORK(35),SPACES                                                  
         BE    ADDR13X                                                          
         MVC   P+64(32),WORK                                                    
         B     ADDR13P                                                          
*                                                                               
ADDR13X  BAS   RE,PRINTITR         SKIP A LINE                                  
         B     ADDR3               NEXT ADDRESS RECORD OR ELEMENT               
*                                                                               
ADDR14   DS    0H                  LOOK FOR ADDRESS ELEMENTS                    
         LA    R4,PUBREC+33                                                     
ADDR14B  CLI   0(R4),0             END OF RECORD ?                              
         BE    ADDR15              YES                                          
         CLC   ELCODE1,0(R4)       DESIRED ADDRESS ELEMENT ?                    
         BE    ADDR6               YES - GO CHECK IT                            
ADDR14D  ZIC   R0,1(R4)            NEXT ELEMENT                                 
         AR    R4,R0                                                            
         B     ADDR14B                                                          
*                                                                               
ADDR15   DS    0H                                                               
         MVC   AREC,FULL           RESTORE PPG'S AREC                           
         CLI   INFSW,1             SEE IF ADDR INFO PRINTED                     
         BE    DSPCE               YES - MAY NEED TO DOUBLE SPACE               
         SP    PUBCNT,=P'1'        ADJUST PUB COUNT                             
         B     PREXT               ELSE PRINT NOTHING                           
*                                                                               
         EJECT                                                                  
ADDR50   DS    0H               HERE IF CHECKING ALL SOURCES                    
*                               NOTE  - MUST BE ONE CLIENT                      
         XC    WORK(10),WORK                                                    
         MVC   CLTAGY,QAGENCY                                                   
         MVC   CLTMED,QMEDIA                                                    
         MVC   CLTCODE,QCLIENT                                                  
         MVC   CLTOFF,PCLTOFF                                                   
         CLI   QCLIENT,C'*'      SEE IF LOOKING FOR OFFICE ADDRESSES            
         BNE   *+10                                                             
         MVC   CLTOFF,QCLIENT+1                                                 
*                                                                               
         MVI   ADRTYP,C'P'                                                      
         CLI   QOPT1,C'1'       PAYING ADDRESS                                  
         BE    ADDR52                                                           
         MVI   ADRTYP,C'T'                                                      
         CLI   QOPT1,C'2'       TRAFFIC ADDRESS                                 
         BE    ADDR52                                                           
         MVI   ADRTYP,C'C'                                                      
         CLI   QOPT1,C'3'       CONTRACT ADDRESS                                
         BE    ADDR52                                                           
         MVI   ADRTYP,C'S'                                                      
         CLI   QOPT1,C'4'       SHIPPING ADDRESS                                
         BE    ADDR52                                                           
         DC    H'0'          BAD LIST TYPE                                      
*                                                                               
ADDR52   DS    0H                                                               
         GOTO1 =V(PPGETADR),DMCB,(ADRTYP,CLTDATA),PUBREC,DATAMGR                
         CLI   0(R1),X'FF'     ERROR                                            
         BNE   *+6                                                              
         DC    H'0'          PPGETADR ERROR?                                    
         CLI   0(R1),0       SEE IF ADDRESS RECORD FOUND                        
         BE    ADDR100       GO CHECK FOR REP                                   
*                                  ADDRESS REC FOUND                            
         MVC   WORK(4),0(R1)       "RESULT"/ADDRESS LEVEL (2-4)                 
         L     R2,4(R1)            A(ADDRESS INFO FROM CALL)                    
         ST    R2,FULL             SAVE A(ADDRESS)                              
*                                                                               
         CLI   WORK,X'0B'          SEE IF I WAS LOOKING FOR SHIPPING            
         BE    RREP20              AND FOUND IT                                 
*                                                                               
****                                                                            
****     CODE BELOW COULD BE REPLACED BY A NEW MODULE                           
****     PPGETREP THAT WOULD WORK LIKE PPGETADR                                 
****                                                                            
*                                  NOW TRY FOR A REP                            
ADDR100  DS    0H                                                               
*                                                                               
RREP6    DS    0H                                                               
         CLI   QOPT1,C'4'         SHIPPING?                                     
         BE    RREP20             DON'T CHECK FOR REP                           
*                                                                               
         LA    R2,PUBREC+33                                                     
         XC    DUB,DUB                                                          
         SR    R0,R0                                                            
RREP7    DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    RREP11                                                           
         CLI   0(R2),X'14'                                                      
         BE    RREP9                                                            
RREP8    DS    0H                                                               
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     RREP7                                                            
RREP9    DS    0H                                                               
         USING PUBREPEL,R2                                                      
         CLC   PUBRPOFF,=3X'FF'                                                 
         BE    RREP10                                                           
         CLC   PUBRPOFF,QCLIENT                                                 
         BE    RREP10                                                           
         CLI   PUBRPOFF,X'FF'                                                   
         BNE   RREP8                                                            
         CLI   QCLIENT,C'*'         SEE IF LOOKING FOR OFFICE                   
         BNE   RREP9C                                                           
         CLC   PUBRPOFF+1(1),QCLIENT+1                                          
         BE    RREP10                                                           
         B     RREP8                                                            
*                                                                               
RREP9C   CLC   PUBRPOFF+1(1),PCLTOFF                                            
         BNE   RREP8                                                            
RREP10   DS    0H                                                               
*                                                                               
RREP10A2 CLI   WORK+1,0                WILL BE 0 IF NO PAY ADDR FND             
         BE    RREP10A3                                                         
         CLI   WORK+1,X'FF'            SEE IF I FOUND CLIENT ADDRESS            
         BL    RREP20                  DON'T LOOK FOR REP                       
RREP10A3 OC    PUBPAREP(12),PUBPAREP                                            
         BZ    RREP8               NO OVERIDES THIS ELEM                        
         LA    RF,PUBPAREP                                                      
         CLI   QOPT1,C'1'                                                       
         BE    RREP10B                                                          
         LA    RF,PUBTRREP                                                      
         CLI   QOPT1,C'2'                                                       
         BE    RREP10B                                                          
         LA    RF,PUBCNREP                                                      
         CLI   QOPT1,C'3'                                                       
         BE    RREP10B                                                          
         DC    H'0'          SOMETING WRONG - BAD LIST TYPE                     
RREP10B  DS    0H                                                               
         MVC   DUB(4),0(RF)                                                     
         MVC   DUB+4(3),PUBRPOFF                                                
RREP11   DS    0H                                                               
         OC    WORK+1(3),WORK+1                                                 
         BZ    RREP12                                                           
         CLI   QOPT1,C'1'          TEST PAY ADDRESS                             
         BNE   RREP11B                                                          
         CLC   WORK+1(3),DUB+4     TEST 'LEVEL'                                 
         BL    RREP20              IF ADDR MORE SPECIFIC USE IT                 
         B     RREP12              ELSE USE REP                                 
*                                                                               
RREP11B  CLC   WORK+1(3),DUB+4     TEST 'LEVEL'                                 
         BNH   RREP20              ADDR MORE SPECIFIC OR EQUAL                  
*                                  - USE ADDR                                   
RREP12   DS    0H                                                               
         OC    DUB(4),DUB                                                       
         BZ    RREP20              NO REP                                       
         MVC   WORK+1(3),DUB+4     SET 'LEVEL' IN WORK                          
*                                                                               
         LR    R0,RE                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(2),PUBKEY+7     AGENCY                                       
         MVC   KEY+2(1),PUBKEY     MEDIA                                        
         MVI   KEY+3,X'11'                                                      
         MVC   KEY+4(4),DUB        REP CODE                                     
         CLC   KEY(10),PREPKEY     DONT READ IF ALREADY THERE                   
         BE    RREP18                                                           
         XC    PREPKEY(159),PREPKEY                                             
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(10),KEY                                                  
         BNE   RREP18                                                           
         GOTO1 GETREP                                                           
         B     RREP19                                                           
*                                                                               
RREP18   DS    0H                                                               
         B     RREPX               JUST EXIT SINCE I'M AT LBUYREQ               
RREP19   B     RREPX               JUST EXIT SINCE I'M AT LBUYREQ               
         SPACE 3                                                                
RREP20   DS    0H                                                               
         XC    PREPREC(197),PREPREC                                             
         XC    REMAIL,REMAIL        CLEAR AREA FOR EMAIL                        
         CLI   WORK+1,0                                                         
         BE    RREPX            NO ADDR ELEM                                    
         L     R2,FULL                                                          
         USING PGETADRD,R2                                                      
         MVC   PREPELEM(2),PGADELEM                                             
         MVC   PREPNAME(122),PGADNAME                                           
*                                                                               
         XC    PREPFAX,PREPFAX            CLEAR                                 
         XC    PREPLIN3,PREPLIN3                                                
         MVC   REMAIL,PGADEADD     EMAIL ADDRESS                                
         MVC   PREPLIN3(26),PGADLIN3                                            
         MVC   PREPFAX(12),PGADFAX                                              
*                                                                               
RREP20P  MVC   PREPKEY(3),RCSVAGY                                               
*                                                                               
         DROP  R2                                                               
*                                                                               
RREPX    DS    0H                                                               
         CLI   PREPNAME,0                                                       
         BH    RREPX8                                                           
*                               NO ADDRESS OR REP FOUND                         
*                               USE MAIN PUB ADDRESS                            
         MVC   P+127(5),=C'*DIR*'       DIRECT TO PUB                           
         MVC   P+97(20),PUBNAME                                                 
         CLI   PUBZNAME,C' '                                                    
         BNH   RREPX2                                                           
         CLI   QMEDIA,C'O'   FOR OUTDOOR ZONE IS USALLY MARKET                  
         BE    RREPX2                                                           
         MVC   PSECOND+97(20),PUBZNAME                                          
         BAS   RE,PRINTITR                                                      
         B     RREPX2B                                                          
*                                                                               
*        IF NOT PRINTING ZONE  USE PSECOND FOR PUBLINE1                         
RREPX2   MVC   PSECOND+97(30),PUBLINE1                                          
         BAS   RE,PRINTITR                                                      
         MVC   P+97(30),PUBLINE2                                                
         BAS   RE,GETDATA                                                       
         MVC   P+64(32),WORK                                                    
         BAS   RE,PRINTITR                                                      
         B     RREPX2X                                                          
*                                                                               
RREPX2B  MVC   P+97(30),PUBLINE1                                                
         BAS   RE,GETDATA                                                       
         MVC   P+64(32),WORK                                                    
         MVC   PSECOND+97(30),PUBLINE2                                          
         BAS   RE,GETDATA                                                       
         MVC   PSECOND+64(32),WORK                                              
         BAS   RE,PRINTITR                                                      
RREPX2X  CLI   SVPATTN,C' '                                                     
         BNH   RREPX3                                                           
         MVC   P+97(30),SVPATTN                                                 
         BAS   RE,GETDATA                                                       
         MVC   P+64(32),WORK                                                    
         BAS   RE,PRINTITR                                                      
*                                                                               
RREPX3   CLI   SVPTELN,C' '                                                     
         BNH   RREPX4                                                           
         MVC   P+97(20),SVPTELN                                                 
         BAS   RE,GETDATA                                                       
*                                                                               
RREPX3X  BAS   RE,PRINTITR                                                      
RREPX4   CLI   SVPFAXN,C' '                                                     
         BNH   RREPX6                                                           
         MVC   P+97(30),SVPFAXN                                                 
         BAS   RE,GETDATA                                                       
         MVC   P+64(32),WORK                                                    
         CLC   SVPTOFAX,SPACES     PRESENT FOR FX= (FAX RECORDS)                
         BE    RREPX5X                                                          
         MVI   P+112,C'('                                                       
         MVC   P+113(16),SVPTOFAX                                               
         LA    R1,P+129                                                         
RREPX5A  CLI   0(R1),C' '                                                       
         BH    RREPX5B                                                          
         BCT   R1,RREPX5A                                                       
RREPX5B  MVI   1(R1),C')'                                                       
RREPX5X  BAS   RE,PRINTITR                                                      
RREPX6   BAS   RE,GETDATA                                                       
         CLC   WORK(35),SPACES                                                  
         BE    RREPX7                                                           
         MVC   P+64(32),WORK                                                    
         BAS   RE,PRINTITR                                                      
         B     RREPX6             GO CHECK FOR MORE DATA                        
*                                                                               
RREPX7   BAS   RE,DSPCE                                                         
         B     PREXT            DONE                                            
         EJECT                                                                  
RREPX8   DS    0H                                                               
         LA    R5,P+128                                                         
         MVC   0(3,R5),=C'ALL'                                                  
         CLC   WORK+1(3),=3X'FF'                                                
         BE    RREPX9                                                           
         MVC   0(3,R5),WORK+1                                                   
         CLI   WORK+1,X'FF'                                                     
         BNE   RREPX9                                                           
         MVI   0(R5),C'*'                                                       
*                                                                               
*        PRINT OFFICE CODE                                                      
*                                                                               
         GOTOR VPRNTOFC,DMCB,WORK+2,1(R5),VOFFICER,QAGENCY,VCOMFACS             
*                                                                               
*                                                                               
RREPX9   MVI   P+131,C'R'                                                       
         CLI   PREPREC+3,X'11'        SEE IF REALLY A REP                       
         BE    *+8                                                              
         MVI   P+131,C' '             MUST BE ADDRESS RECORD                    
*                                                                               
         LA    R5,P+97                                                          
         MVC   000(30,R5),PREPNAME                                              
         MVC   132(30,R5),PREPLIN1                                              
         BAS   RE,PRINTITR                                                      
         MVC   0(30,R5),PREPLIN2                                                
         BAS   RE,GETDATA                                                       
         MVC   P+64(32),WORK                                                    
         BAS   RE,PRINTITR                                                      
         CLC   PREPATTN,SPACES        CHECK FOR REP ATTN                        
         BE    PREP7D                                                           
         MVC   0(5,R5),=C'ATTN-'                                                
         MVC   5(20,R5),PREPATTN                                                
         BAS   RE,GETDATA                                                       
         MVC   P+64(32),WORK                                                    
         BAS   RE,PRINTITR                                                      
*                                                                               
PREP7D   CLC   PREPTEL,SPACES      CHECK FOR PHONE NUMBER                       
         BNH   PREP7T                                                           
         MVC   000(04,R5),=C'TEL-'                                              
         MVC   004(12,R5),PREPTEL                                               
         BAS   RE,GETDATA                                                       
         MVC   P+64(32),WORK                                                    
         BAS   RE,PRINTITR                                                      
*                                                                               
PREP7T   OC    PREPFAX,PREPFAX     FAX?                                         
         BZ    PREP7EM                                                          
         MVC   000(04,R5),=C'FAX-'                                              
         MVC   004(12,R5),PREPFAX                                               
         CLC   =C'FX=',PREPFAX     NUMBER IN CONTROL FILE ?                     
         BNE   PREP7TD             NO                                           
         MVC   FRSVFAX,PREPFAX                                                  
         OC    FRSVFAX,SPACES                                                   
         GOTO1 AGETFAX             GET FAX FROM CONTROL FILE                    
         MVI   015(R5),C'('                                                     
         MVC   016(16,R5),TOFAX                                                 
         LA    R1,31(R5)                                                        
PREP7TA  CLI   0(R1),C' '                                                       
         BH    PREP7TB                                                          
         BCT   R1,PREP7TA                                                       
PREP7TB  MVI   1(R1),C')'                                                       
*                                                                               
*                                                                               
PREP7TD  BAS   RE,GETDATA                                                       
         MVC   P+64(32),WORK                                                    
         BAS   RE,PRINTITR                                                      
*                                                                               
PREP7EM  DS    0H                                                               
         OC    REMAIL,REMAIL   SEE IF I HAVE AN EMAIL ADDRESS                   
         BZ    PREP8                                                            
         MVC   0(06,R5),=C'EMAIL-'                                              
         MVC   6(25,R5),REMAIL                                                  
         BAS   RE,GETDATA                                                       
         MVC   P+64(32),WORK                                                    
         BAS   RE,PRINTITR                                                      
*                                                                               
         CLC   REMAIL+25(5),SPACES                                              
         BNH   PREP8                                                            
         MVC   6(25,R5),REMAIL+25                                               
         BAS   RE,GETDATA                                                       
         MVC   P+64(32),WORK                                                    
         BAS   RE,PRINTITR                                                      
         CLC   REMAIL+50(5),SPACES                                              
         BNH   PREP8                                                            
         MVC   6(10,R5),REMAIL+50                                               
         BAS   RE,GETDATA                                                       
         MVC   P+64(32),WORK                                                    
         BAS   RE,PRINTITR                                                      
PREP8    DS    0H                                                               
*                                                                               
         BAS   RE,GETDATA                                                       
         MVC   P+64(32),WORK                                                    
RREP8P   BAS   RE,PRINTITR                                                      
         BAS   RE,GETDATA     MUST BE SURE I'VE PRINTED EVERYTHING              
         CLC   WORK(35),SPACES                                                  
         BE    RREP8X                                                           
         MVC   P+64(32),WORK                                                    
         B     RREP8P                                                           
*                                                                               
RREP8X   BAS   RE,DSPCE                                                         
         B     PREXT               NEXT ADDRESS RECORD OR ELEMENT               
         DROP  R4,R5                                                            
         EJECT                                                                  
*                               PUB GROUP ASSIGNMENTS                           
PGA      DS    0H                                                               
         MVI   INFSW,0                                                          
         MVI   RCSUBPRG,24                                                      
         CLI   QOPT7,C' '          FILTER ON GROUP ID (SCHEME) ?                
         BE    PGAGO               NO                                           
         MVI   RCSUBPRG,25                                                      
         MVC   HEAD4+73(1),QOPT7                                                
PGAGO    MVC   PSECOND,SPACES      NO 2ND LINE                                  
         MVC   FULL,AREC           SAVE PPG'S AREC                              
         XC    KEY,KEY                                                          
         LA    R4,KEY              ---------------------                        
         USING GRPPKEY,R4          BUILD PASSIVE POINTER                        
         MVC   GRPPAGY,PUBKAGY                                                  
         MVC   GRPPMED,PUBKMED                                                  
****     MVC   GRPPCODE,=X'0000'                                                
         MVI   GRPPTYP,GRPPBGQ     PUB GROUP                                    
         MVC   GRPPVAL(6),PUBKPUB                                               
         MVC   SAVEPKEY,GRPPKEY                                                 
*                                                                               
         GOTO1 HIGH                GET FIRST PASSIVE POINTER                    
         CLC   SAVEPKEY(13),GRPPKEY    SAME PUB ?                               
         BE    PGALOOP             YES                                          
*                                                                               
         CLI   QOPT6,C'Y'         SEE IF REPORTING UNASSGINED                   
         BE    PGAG5                                                            
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         B     PGAXIT                                                           
*                                                                               
PGAG5    MVC   P+63(18),=C'*** UNASSIGNED ***'                                  
         BAS   RE,PRINTITR                                                      
         MVI   INFSW,1                                                          
         B     PGAXIT                                                           
*                                                                               
PGALOOP  MVI   INFSW,1                 SO PUB COUNT IS CORRECT                  
         CLC   SAVEPKEY(13),GRPPKEY    SAME PUB ?                               
         BNE   PGAXIT              NO - DONE                                    
         CLI   QOPT7,C' '          FILTER ON GROUP ID (SCHEME) ?                
         BE    PGAL10              NO                                           
         CLC   GRPPID,QOPT7        GROUP ID (SCHEME) MATCHED ?                  
         BNE   PGA40               NO - NEXT PASSIVE POINTER                    
PGAL10   MVC   SAVEPKEY,GRPPKEY                                                 
         LA    R5,P+63             LINE AREA                                    
         USING PGALIND,R5                                                       
         MVC   PGALIN,SPACES                                                    
*                                                                               
         MVC   SAVEID,GRPPID       GROUP ID/CODE FROM PASSIVE PTR               
         MVC   SAVECODE,GRPPCODE   XL2 PWOS                                     
         ICM   R2,B'1100',GRPPCODE        FROM PASSIVE PTR                      
         SRL   R2,12                DD DD ?? ??  =>  00 0D DD D?                
         ST    R2,FULL                                                          
         OI    FULL+3,X'0F'         00 0D DD DS                                 
         UNPK  CODECHAR(5),FULL+1(3)             =>  Z0 ZD ZD ZD ZD             
         DROP  R4                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GRPKEY,R4           BUILD GROUP DEFINITION KEY                   
         MVC   GRPKAGY,PUBKAGY                                                  
         MVC   GRPKMED,PUBKMED                                                  
         MVC   GRPKID,SAVEID       ID FROM PASSIVE PTR                          
****     MVC   GRPKCODE,=X'0000'   GROUP DEF RECORD                             
         MVI   GRPKRCOD,GRPKBTYQ   PUB GROUP                                    
         MVC   SAVEKKEY,GRPKEY                                                  
*                                                                               
         GOTO1 HIGH                GET GROUP DEFINITION RECORD                  
*                                                                               
         CLC   SAVEKKEY(10),GRPKEY                                              
         BE    *+6                 GET IT?                                      
         DC    H'0'                NO - VERY BAD NEWS                           
*                                                                               
******** LA    R4,PCONREC          I/O FOR GROUP DEFINITION RECORD              
         L     R4,ACONIO1          I/O FOR GROUP DEFINITION RECORD              
         ST    R4,AREC                                                          
         GOTO1 GETPRT                                                           
         LA    R4,33(R4)                                                        
         USING GRPBRKD,R4                                                       
         CLI   GRPBRKCD,GRPBRKCQ      BREAK DESCRIPTION ELEMENT                 
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         ZIC   R2,GRPBK1LN         L'BREAK CODES                                
         ZIC   R0,GRPBK2LN                                                      
         AR    R2,R0               L'WHOLE GROUP CODE                           
         BCTR  R2,0                                                             
         EX    R2,PGAEX                                                         
         B     PGA10                                                            
PGAEX    MVC   PGACODE(0),CODECHAR+1    CODE TO LINE BLANK PADDED               
*                                                                               
PGA10    MVC   PGABRK1,GRPBK1      BREAK TITLES TO LINE                         
         OC    GRPBK2,GRPBK2       MAY BE ONLY ONE                              
         BZ    PGA20                                                            
         MVC   PGABRK2,GRPBK2                                                   
         DROP  R4                                                               
*                                                                               
PGA20    XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GRPKEY,R4           BUILD GROUP RECORD KEY                       
         MVC   GRPKAGY,PUBKAGY                                                  
         MVC   GRPKMED,PUBKMED                                                  
         MVC   GRPKID,SAVEID       ID/CODE FROM PASSIVE POINTER                 
         MVC   GRPKCODE,SAVECODE   GROUP RECORD                                 
         MVI   GRPKRCOD,GRPKBTYQ   PUB GROUP                                    
         MVC   SAVENKEY,GRPKEY                                                  
*                                                                               
         GOTO1 HIGH                GET GROUP RECORD                             
*                                                                               
         CLC   SAVENKEY(10),GRPKEY                                              
         BE    *+6                 NOT THERE?                                   
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
******** LA    R4,PCONREC          I/O FOR GROUP CODE RECORD                    
         L     R4,ACONIO1          I/O FOR GROUP CODE RECORD                    
         ST    R4,AREC                                                          
         GOTO1 GETPRT                                                           
         LA    R4,33(R4)                                                        
         USING GRPGRPD,R4                                                       
         CLI   GRPGRPCD,GRPGRPCQ      BREAK NAMES ELEMENT                       
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         MVC   PGANAME1,GRPGNAM1      GROUP NAMES TO LINE                       
         OC    GRPGNAM2,GRPGNAM2                                                
         BZ    PGA30                                                            
         MVC   PGANAME2,GRPGNAM2                                                
*                                                                               
PGA30    DS    0H                  PRINT THE LINE                               
         MVC   PGAID,SAVEID        GROUP ID TO LINE                             
         BAS   RE,PRINTITR                                                      
         MVI   INFSW,1                                                          
         MVC   P,SPACES                                                         
         DROP  R4,R5                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),SAVEPKEY    RESTORE KEY TO PASSIVE PTR                   
         GOTO1 HIGH                                                             
         CLC   SAVEPKEY(25),KEY                                                 
         BE    PGA40                                                            
         DC    H'0'                                                             
*                                                                               
PGA40    GOTO1 SEQ                 NEXT PASSIVE POINTER                         
         LA    R4,KEY                                                           
         B     PGALOOP                                                          
*                                                                               
PGAXIT   DS    0H                                                               
         MVC   AREC,FULL           RESTORE PPG'S AREC                           
         CLI   INFSW,1             SEE IF PGA INFO PRINTED                      
         BE    PREXT               YES                                          
         SP    PUBCNT,=P'1'        NO - ADJUST PUB COUNT                        
         B     PREXT                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
         DS    F                                                                
PRINTIT  LA    RE,DSPCE           ROUTINES GO TO PRINTIT WHEN DONE              
PRINTITR ST    RE,PRINTIT-4                                                     
         SP    PCOUNT,=P'1'                                                     
         BNP   PRINTXX                                                          
         CLI   QOPT2,C'Y'                                                       
         BNE   *+10                                                             
         MVC   HEAD7+1(20),=C'* AGENCY PUBS ONLY *'                             
*                                                                               
         CLI   QOPT1,C'V'         CHK IF ADV-AOR LISTING                        
         BNE   PRINT2                                                           
         MVC   HEAD4+1(11),=C'ADVERTISER='                                      
         MVC   HEAD4+13(3),QCLIENT                                              
         B     PRINT3                                                           
*                                                                               
PRINT2   CLI   FILTCLT,0          SEE IF CLIENT FILTER SPECIFIED                
         BE    PRINT3                                                           
         CLI   QCLIENT,C'*'                                                     
         BNE   PRINT2C                                                          
         MVC   HEAD4+1(7),=C'OFFICE='                                           
*SMY*    MVC   HEAD4+9(1),QCLIENT+1                                             
*                                                                               
*        PRINT OFFICE CODE                                                      
*                                                                               
         GOTOR VPRNTOFC,DMCB,QCLIENT+1,(C'L',HEAD4+10),VOFFICER,       X        
               QAGENCY,VCOMFACS                                                 
*                                                                               
         B     PRINT3                                                           
*                                                                               
PRINT2C  MVC   HEAD4+1(7),=C'CLIENT='                                           
         MVC   HEAD4+9(3),FILTCLT                                               
*                                                                               
PRINT3   CLI   QOPT4,C'Y'                                                       
         BNE   *+10                                                             
         MVC   HEAD5+1(36),=C'* PUBS WITH BUYS OR CONTRACTS ONLY *'             
         CLI   QOPT4,C'B'                                                       
         BNE   *+10                                                             
         MVC   HEAD5+1(23),=C'* PUBS WITH BUYS ONLY *'                          
         CLI   QOPT4,C'C'                                                       
         BNE   *+10                                                             
         MVC   HEAD5+1(28),=C'* PUBS WITH CONTRACTS ONLY *'                     
         CLI   QOPT4,C'L'                                                       
         BNE   *+10                                                             
         MVC   HEAD5+1(20),=C'* LOCKED PUBS ONLY *'                             
         CLI   QOPT4,C'U'                                                       
         BNE   *+10                                                             
         MVC   HEAD5+1(22),=C'* UNLOCKED PUBS ONLY *'                           
*                                                                               
         CLI   QOPT5,C'E'     LANGUAGE FILTER                                   
         BNE   *+10                                                             
         MVC   HEAD6+1(30),=C'* ENGLISH LANGUAGE PUBS ONLY *'                   
         CLI   QOPT5,C'F'                                                       
         BNE   *+10                                                             
         MVC   HEAD6+1(29),=C'* FRENCH LANGUAGE PUBS ONLY *'                    
*                                                                               
         CLC   QPUB+1(2),=C'L='    PUB LIST                                     
         BNE   PRINT3B                                                          
         MVC   HEAD6+43(9),=C'PUB LIST='                                        
         MVC   HEAD6+53(3),QPUB+3                                               
         MVC   HEAD6+57(20),SVLSTN    SAVED PUBLIST NAME                        
*                                                                               
PRINT3B  CLC   QPUB+1(2),=C'P='    PUBLISHER PUB LIST                           
         BNE   PRINT4                                                           
         MVC   HEAD6+45(10),=C'PUBLISHER='                                      
         MVC   HEAD6+55(4),QPUB+3                                               
         MVC   HEAD6+61(30),PBLNAME   SAVED PUBLISHER NAME                      
*                                                                               
PRINT4   OC    KILLDATE,KILLDATE                                                
         BZ    PRINT5                                                           
         MVC   HEAD7+1(52),=C'* NO PUBS WHOSE KILL DATE IS ON OR BEFOREX        
                         *'                                                     
         MVC   HEAD7+43(8),KILLDATP                                             
*                                                                               
PRINT5   SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,LINE                                                          
         IC    RF,LNEED                                                         
         AR    RE,RF                                                            
         STC   RE,BYTE                                                          
         CLC   BYTE,MAXLINES                                                    
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVI   LNEED,0                                                          
*                                                                               
*                                                                               
PRINTX   GOTO1 REPORT                                                           
PRINTXX  L     RE,PRINTIT-4                                                     
         BR    RE                                                               
         SPACE 2                                                                
DSPCE    DS    0H                                                               
         CLI   QOPT3,C'Y'          SEE IF DOUBLE SPACING                        
         BNE   PREXT                                                            
         SR    RE,RE                                                            
         IC    RE,LINE                                                          
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
         CLC   BYTE,MAXLINES                                                    
         BH    PREXT                                                            
         GOTO1 REPORT                                                           
PREXT    XIT1                                                                   
         SPACE 2                                                                
         EJECT                                                                  
*                                                                               
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0           END OF RECORD                                  
         BE    NEXTEL2                                                          
         CLC   ELCODE1,0(R4)                                                    
         BER   RE                                                               
*                                                                               
         B     NEXTEL+2                                                         
*                                                                               
NEXTEL2  LTR   RE,RE                                                            
         BR    RE               RETURN CC NOT EQUAL                             
*                                                                               
         LTORG                                                                  
*                                                                               
REMAIL   DS    CL60           WORKING EMAIL                                     
         EJECT                                                                  
         DC    F'0'                                                             
GETDATA  ST    RE,GETDATA-4    USED TO GET PUB DATA FOR ADDRESS                 
*                              LISTINGS                                         
         MVC   WORK(35),SPACES     RETURNED IN WORK                             
         TM    PPSW,X'01'      SEE IF ATTN PRINTED                              
         BO    GETD2                                                            
         OI    PPSW,X'01'      SET SVPATTN PRINTED                              
         MVC   WORK(30),SVPATTN                                                 
         CLI   SVPATTN,C' '                                                     
         BNE   GETDX                                                            
*                                                                               
GETD2    TM    PPSW,X'02'      SEE IF TELE PRINTED                              
         BO    GETD4                                                            
         MVC   WORK(20),SVPTELN      TRY FOR TELE                               
         OI    PPSW,X'02'      SET SVPTLN PRINTED                               
         CLI   SVPTELN,C' '                                                     
         BNE   GETDX                                                            
*                                                                               
GETD4    TM    PPSW,X'04'      SEE IF FAX PRINTED                               
         BO    GETDX                                                            
         MVC   WORK(30),SVPFAXN                                                 
         OI    PPSW,X'04'       SET FAX PRINTED                                 
         CLI   SVPFAXN,C' '                                                     
         BNH   GETDX                                                            
         CLC   SVPTOFAX,SPACES                                                  
         BE    GETDX                                                            
         MVI   WORK+14,C'('                                                     
         MVC   WORK+15(16),SVPTOFAX                                             
         LA    R1,WORK+30                                                       
GETD20   CLI   0(R1),C' '                                                       
         BH    GETD21                                                           
         BCT   R1,GETD20                                                        
GETD21   MVI   1(R1),C')'                                                       
*                                                                               
GETDX    L     RE,GETDATA-4                                                     
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*                               GET FAX NUMBER FROM CONTROL FILE                
GETFAX   CSECT                                                                  
         NMOD1 0,GETFAX                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC,R9                                                    
         USING PP48WRKD,R7                                                      
         SPACE 2                                                                
         MVC   TOFAX,SPACES                                                     
         MVC   SAVEPKEY,KEY       SAVE KEY                                      
         MVC   SAVEKKEY,KEYSAVE   SAVE KEYSAVE                                  
GETFAXB  XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTFXKEY,R4                                                       
         MVI   CTFXKTYP,CTFXEQU                                                 
         MVC   CTFXAGY,QAGENCY                                                  
         MVC   CTFXCODE,FRSVFAX+3                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI  ',=C'CTFILE ',KEY,PBUYREC                
         CLC   PBUYREC(18),KEYSAVE      COMPARE 7-BYTE FAX CODE                 
         BNE   GETFAXN                                                          
         LA    R4,PBUYREC       PBUYREC BEING USED FOR CONTROL FILE I/O         
         LA    R5,CTFXEL1                                                       
         B     GETFX4                                                           
         SPACE 1                                                                
GETFX2   ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         SPACE 1                                                                
GETFX4   CLI   0(R5),0                                                          
         BE    GETFAXN                                                          
         CLI   0(R5),CTFX1ELQ                                                   
         BE    GETFXNO                                                          
         B     GETFX2                                                           
         SPACE 1                                                                
FAX      USING CTFX1EL,R5                                                       
GETFXNO  ZIC   R1,FAX.CTFX1LEN     FAX NUMBER                                   
         SH    R1,=H'3'                                                         
         CH    R1,=H'24'                                                        
         BL    *+8                                                              
         LA    R1,24                                                            
         EX    R1,*+8                                                           
         B     GETFAXX                                                          
         MVC   TOFAX(0),FAX.CTFX1NUM                                            
         SPACE 1                                                                
*                                                                               
GETFAXN  MVC   TOFAX,=C'** NOT FOUND ** '                                       
*                                                                               
GETFAXX  MVC   KEY(32),SAVEPKEY        RESTORE KEY                              
         MVC   KEYSAVE(32),SAVEKKEY      AND KEYSAVE                            
         XIT1                                                                   
         DROP  R4,FAX                                                           
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
PP48WRKD DSECT                                                                  
PP48WRK  DS    0C                                                               
AGETFAX  DS    A                                                                
AOUTER   DS    A                                                                
LNEED    DS    X                                                                
METH     DS    X                                                                
PPSW     DS    CL1                                                              
PROF48   DS    CL16                                                             
*                                                                               
ELCODE1  DS    CL1                                                              
MYSVMED  DS    CL1                 SAVED REQUESTED MEDIA                        
INFSW    DS    CL1                                                              
ADDRSW   DS    CL1                 ADDRESS RECORD OR ELEMENT SWITCH             
REPACT   DS    CL1                                                              
PAYCNT   DS    PL3                 FOR "PAY" SECTION PRINTING                   
PCOUNT   DS    PL5                                                              
PUBCNT   DS    PL3                 NUMBER OF PUBS LISTED                        
FILTCLT  DS    XL3                                                              
KILLDATE DS    XL3                 SKIP PUBS WHOSE KILL DATE                    
*                                  IS ON OR BEFORE THIS DATE                    
KILLDATP DS    XL8                 FOR PRINTING                                 
*                                                                               
ADRTYP   DS    CL1                                                              
CLTDATA  DS    0CL7                                                             
CLTAGY   DS    CL2                                                              
CLTMED   DS    CL1                                                              
CLTCODE  DS    CL3                                                              
CLTOFF   DS    CL1                                                              
*                                                                               
         DS    CL8                 SPARE                                        
SCFILTER DS    CL2                 STATE CODE FILTER (RCARD2 COL 60-61)         
*                                                                               
APSTVAL  DS    A                   ADDR OF PSTVAL                               
VPRNTOFC DS    V                   ADDR OF PRNTOFC                              
VOFFICER DS    V                   ADDR OF OFFICER                              
*                                                                               
SAVERF   DS    F                                                                
SAVEREG  DS    F                                                                
ATTN     DS    CL30                                                             
TELN     DS    CL20                                                             
FAXN     DS    CL30                                                             
TOFAX    DS    CL16                FAX NUMBER FROM CONTROL FILE                 
FRSVFAX  DS    CL12                FAX IN FORM "FX=XXXXXXX"                     
PBLNAME  DS    CL30                USED TO SAVE PUBLISHER NAME                  
SVLSTN   DS    CL20                USED TO SAVE PUBLIST NAME                    
*                                                                               
SVPATTN  DS    CL30                SAVED FROM PUB                               
SVPTELN  DS    CL20                                                             
SVPFAXN  DS    CL30                                                             
SVPTOFAX DS    CL16                                                             
*                                                                               
EMAIL1   DS    CL25                USED TO SPLIT UP 60-BYTE                     
EMAIL2   DS    CL25                  EMAIL ADDRESS                              
EMAIL3   DS    CL25                                                             
*                                                                               
PSTBLK   DS    CL(PSTLNQ)                                                       
*                                                                               
PSTOUT   DS    CL64                                                             
*                                                                               
PBYOWK   DS    CL600                                                            
*                                                                               
SAVEID   DS    C                   GROUP ID                                     
SAVECODE DS    XL2                 GROUP CODE (PWOS)                            
CODECHAR DS    XL5                 UNPK AREA                                    
SAVEPKEY DS    XL32                PASSIVE POINTER - PUB GRP ASSIGNS            
SAVEKKEY DS    XL32                PUB GROUP ID DEFINITION                      
SAVENKEY DS    XL32                PUB GROUP CODE                               
PMYKEY   DS    XL32                                                             
PMYKSAVE DS    XL32                                                             
*                                                                               
*                                                                               
*                                                                               
RTLIND   DSECT                     RATE LINE                                    
         DS    CL63                                                             
RTDAT    DS    CL8                                                              
         DS    CL1                                                              
RTRAT    DS    CL18                                                             
         DS    CL1                                                              
RTDLEV   DS    CL8                                                              
         DS    CL2                                                              
RTDRAT   DS    CL10                                                             
         DS    CL2                                                              
         SPACE 2                                                                
PRLIND   DSECT                     PREM LINE                                    
         DS    CL64                                                             
PRTYP    DS    CL2                                                              
         DS    CL1                                                              
PRCLT    DS    CL3                                                              
         DS    CL1                                                              
PRDAT    DS    CL8                                                              
         DS    CL1                                                              
PRMINS   DS    CL4                                                              
         DS    CL2                                                              
PRMINC   DS    CL8                                                              
         DS    CL1                                                              
PRTYPC   DS    CL7                                                              
         DS    CL1                                                              
PRCHARGE DS    CL9                                                              
         DS    CL2                                                              
PRLINES  DS    CL10                                                             
         SPACE 2                                                                
CLLIND   DSECT                                                                  
         DS    CL75                                                             
CLSPACE  DS    CL6                                                              
         DS    CL9                                                              
CLLINES  DS    CL6                                                              
         SPACE 2                                                                
TAXLIND  DSECT                                                                  
         DS    CL63                                                             
RATEQ    DS    CL10                                                             
TAXRTE   DS    CL7                                                              
         DS    CL14                                                             
DATEQ    DS    CL16                                                             
TAXDTE   DS    CL8                                                              
         SPACE 2                                                                
TAXELD   DSECT                                                                  
ELRTE    DS    XL3                                                              
ELDTE    DS    CL3                                                              
         SPACE 2                                                                
PGALIND  DSECT                   PUB GROUP ASSIGNMENTS LINE                     
PGALIN   DS    0CL69                                                            
PGAID    DS    CL1                                                              
         DS    CL2                                                              
PGACODE  DS    CL4                                                              
         DS    CL2                                                              
PGABRK1  DS    CL11                                                             
         DS    CL1                                                              
PGANAME1 DS    CL18                                                             
         DS    CL1                                                              
PGABRK2  DS    CL11                                                             
         DS    CL1                                                              
PGANAME2 DS    CL17                                                             
         SPACE 2                                                                
PUBGRPLD DSECT                                                                  
       ++INCLUDE PUBGRPEL                                                       
         SPACE 2                                                                
PUBADVD  DSECT                                                                  
       ++INCLUDE PPPUBADVEL                                                     
         SPACE 2                                                                
PAGEPOOL CSECT                                                                  
         DC    F'0'                NUMBER OF BLOCKS USED                        
         DC    F'30'               MAX BLOCKS PER PAGE                          
         DC    F'180'              SIZE OF BLOCK (36*N)                         
         DC    5600C' '                                                         
*                                                                               
PGETADRD DSECT                                                                  
       ++INCLUDE PPGETADRD                                                      
*                                                                               
       ++INCLUDE PGENGRP                                                        
         SPACE 2                                                                
       ++INCLUDE DDPSTBLK                                                       
         EJECT                                                                  
*                                                                               
* NOTE: ELCODE1 AND REP ARE PREVISOULY USED SYMBOLS                             
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2        FOR REQUEST CARD 2                           
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE CTGENFAX                                                       
       ++INCLUDE DDCOMFACSD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'084PPREP4802 04/19/16'                                      
         END                                                                    
