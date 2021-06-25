*          DATA SET PPREP0102S AT LEVEL 029 AS OF 05/01/02                      
*PHASE PP0102A,+0,NOAUTO                                                        
         TITLE 'PP0102 - CHANGE LOG'                                            
*                                                                               
* SMYE  06/00   FIX MINOR BUG IN PKINVM (MATCH RECORD KEY PRINTING)             
*                                                                               
* BPLA   05/00  WHEN DOING A ONE PUB REQ-BE SURE PUB EXITS FIRST                
*                                                                               
* BPLA  04/00   WHEN PURGING PUBS SKIP DELETED CONTRACTS                        
*                                                                               
* SMYE  04/00   ADD DELETE OF CLIENT-SPECIFIC PUB PREMIUM ELEMENTS              
*               (X'60' ELEMENTS) AND THEIR "ASSOCIATED" X'61' PREMIUM           
*               CHARGE ELEMENTS TO HDR PURGES                                   
*                                                                               
* BPLA  03/00   WHEN REQUESTING ONE RECORD SET QOPT2 TO "P"                     
*               SO THAT IT'S USES WILL BE REPORTED                              
*                                                                               
* BPLA  03/00   WHEN PURGING REPS CHECK SPECIAL REP ON ESTIMATES                
*                                                                               
* BPLA  03/00   FIX SEVERAL SMALL BUGS                                          
*               ADD NEW VALUE FOR QOPT3 C= PREVENT PUB PURGES                   
*               IF CONTRACTS ARE FOUND                                          
*                                                                               
* BPLA  03/00   NEW VALUE FOR QOPT2 - G = PRUGEABLE RECORDS ONLY                
*                                                                               
* BPLA  03/00   COPY OF PPREPB102 AT LEVEL 81 MADE 3/08/00                      
*                                                                               
* BPLA 03/00    FIX PRINTING SO SUBHEADING WON'T APPEAR BY                      
*               ITSELF AT THE BOTTOM OF THE PAGE                                
*                                                                               
* BPLA 12/99    WHEN PURGING ADS PURGE BUT DON'T COUNT                          
*               INSTRUCTION RECORDS                                             
*                                                                               
* KWAN 10/99     OPT7=T MEANS RCWRITE=NO                                        
*                                                                               
* KWAN 09/99     SET UP HEADERS BEFORE PRINTING KEYS                            
*                                                                               
* SMYE 4/99      ADDED DELETE OF PUB AD SIZING RECORDS IN PUB PURGES            
*                                                                               
* SMYE 11/30/98  ADDED DELETE OF BILL FORMULA RECORDS TO HDR PURGES             
*                                                                               
* SMYE 4/98      ADDED QOPT1=L FOR PURGE OF PUB LIST RECORDS                    
*                MOVED PUBS TO ITS OWN CSECT                                    
*                ADD JOB RECORD TO PUB PURGE                                    
*                ADD PUB LIST RECORD TO PUB PURGE - DISALLOW PURGE IF           
*                   PUB IN LIST - NO DELETE OF PUB FROM LIST RECORD             
*                                                                               
* BPLA 4/98      WHEN PURGING PUBS PREVENT PURGE IF RECORDS FOUND               
*                FIXES BUG                                                      
*                ALSO ALLOW FOR DELETING OF CONTRACTS IF QOPT 5                 
*                =Y WITH PUB PURGE                                              
*                                                                               
* SMYE 2/98      ADDED QOPT1=S TO ALWAYS DELETE X'25' CLEARANCE STATUS          
*                RECORDS FOR ONE CLIENT ONLY - QCLIENT MUST BE ENTERED          
*                                                                               
* SMYE 1/98      ALTERED COMMENTS TO NEVER (REPEAT NEVER) DELETE THE            
*                   X'40' STANDARD COMMENT RECORD UNLESS QOPT6 = A              
*                   (ALL COMMENTS). IF QOPT6 IS A SINGLE RECORD TYPE            
*                   (LIKE "E"), COMMENTS WILL BE CLEARED FROM THE               
*                   SPECIFIED RECORD TYPE, BUT THE STANDARD COMMENT             
*                   RECORD WILL NOT BE DELETED EVEN IF THE COMMENT              
*                   IS NOT USED IN ANY OTHER RECORD TYPE.                       
*                FIXED DELETE OF COMMENTS FROM PUBREPEL (CM32..)                
*                FIXED DELETE OF BOTH USERP STANDARD COMMENTS (CM31..)          
*                FIX BUG IN CM30.. - WAS NOT PRINTING EST HDRS UNLESS           
*                QOPT6 WAS "A" OR "E"                                           
*                                                                               
* SMYE 12/97     ADDED DELETE OF 2ND ESTIMATE STANDARD COMMENT (CM30..)         
*                                                                               
* SMYE 4/97      ADDED DELETE OF PUB ADDRESS RECORDS IN PUB PURGES              
*                AND IN HDR PURGES                                              
*                                                                               
* BPLA 3/97      FIX BUG IN COMDELB - BUY RECORD MAX LENGTH SET AT              
*                2000 FOR END OF RECORD CLEARING - SHOULD BE 3000               
*                                                                               
* SMYE 10/96     MOVED CKPUB TO ITS OWN CSECT                                   
*                ADDED DELETE OF DRD CLIENT ELEM IN HDR PURGES (HD11)           
*                                                                               
* SMYE  9/96     LOGIC ADDED FOR PURGING ONLY ONE COMMENT, REP, OR JOB          
*                                                                               
* SMYE  7/96     ADDED AOR RECS (X'14') TO COMMENTS PURGE                       
*                                                                               
* SMYE 12/12/95  CHANGED DTCNV TO DATCON WITH NEW PARAM'S                       
*                                                                               
* SMYE 11/95     OPTION INPUT (QPUB) OF ONE PUB NO FOR PURGING (PUBS).          
*        & DEL X'3A/3D' CLNT GRP RECS IF CLT BEING PRGED IS USED(HDRS).         
*        & DEL X'3C/3F' PUB GRP RECS IF PUB BEING PRGED IS USED(PUBS).          
*                                                                               
*                                                                               
* BPLA 6/27/95   ALSO CHECK FOR **PURGE AND ***PURGE                            
*                                                                               
* AROT 11/94     FIX BUG IN COMMENT PURGING.                                    
*                & CK FOR REP CODE IN JOB & BUY RECS (REP PURGE).               
*                & DEL X'29/A/B' RECS IF CLT BEING PRGED IS USED(HDR).          
*                & DEL X'27/8/9/A/B' RECS IF THEY USE PUB BEING PURGED.         
*                                                                               
* BPLA 8/94      FIX BUG IN JOB PURGING                                         
*                                                                               
* SMUR 6/22/94   PURGE COMMENTS FROM JOB, BUY,PRODUCT AND                       
*                ESTIMATE RECORDS.                                              
*                ALSO PURGE CLIENT RECORDS FROM PG COST RECORDS,                
*                SCU RECORDS, FSI RECORDS, EST/PUB UPLOAD RECORDS               
*                                                                BUG01          
* BPLA 2/2/94    IF QOPT2 = C'B' CHECK FOR *PURGE OR PURGE                      
*                                                                               
* BPLA 1/11/94   WHEN PURGING COMMENTS - IF QOPT5 = Y                           
*                REMOVE COMMENT FROM CONTRACTS                                  
*                NOTE THAT IF THE COMMENTS IS USED ELSEWHERE                    
*                IT WILL NOT BE PURGED                                          
*                                                                               
* BPLA 10/13/93 WHEN PURGING REPS DISPLAY 'PUBL' IF REP                         
*               IS USED AS A PUBLISHER                                          
*                                                                               
* BPLA 7/15/93  DISPLAY PUB NAMES OF PUBS BEING PURGED                          
*               IF QCLIENT GIVEN FOR PUB PURGE                                  
*               USE IT TO REPORT X'14' (CLIENT ELEMENTS)                        
*               FOR THAT CLIENT - ** XXX *** WITH PURGE LISTING                 
*                                                                               
* BPLA 1/19/93  DELETE CONTRACTS ON CLIENT PURGES WITH QOPT5 = Y                
*               (EXISTENCE OF CONTRACTS USED TO PREVENT CLIENT                  
*               DELETION)                                                       
* BPLA 6/3/92   ADD CODE FOR DELETING CLEARANCE STATUS RECORDS (X'25)           
*               AND INVOICE MATCHING RECORDS (X'50')                            
* BPLA 9/10/91  SHOW DRD DIV/REG/DST OF PUB ELEMENTS FOR CLIENT PURGE           
*             2 ACTIVITY SWITCHES IN CLIENT TABLE ONE FOR PRTFILE DATA          
*             AND ONE FOR PUBFILE DATA  - NO PURGE IF EITHER IS SET             
*                                                                               
* BPLA 7/24/91  CODE TO DELETE CLIENT ELEMENTS FOR PUBS (QOPT5=Y)               
*                                                                               
* BPLA  7/22/91 ADD CODE TO DELETE RECORDS FOR THE CLIENT                       
*               QOPT5=Y                                                         
* BPLA  3/15/91 QOPT4 LOGIC ADDED TO ALWAYS PRINT KEYS                          
*                                                                               
* BPLA  9/4/90  FOR COMMENT PURGES - CHECK CONTRACT AND I/O                     
*               ELEMENTS                                                        
*                                                                               
* BPLA  9/21/89  LOGIC ADDED TO                                                 
*                1) REP PURGES - CHECK IF USED AS PUBLISHER                     
*                                                                               
*                2) COMMENT PURGES - CHECK IF USED IN CLIENT ELEMS              
*                                    IN PUBFILE                                 
*                                    ALSO CHECK IF USERP RECORDS                
*                                    USE THE COMMENT                            
*                                                                               
*                3) CLIENT PURGES - CHECK FOR AOR RECORDS                       
*                                                                               
* ROSA  6/26/89  LOGIC TO FIND END OF ELEMENT INCORRECT.  BUG    BUG01          
*                WAS DELETING VALID COMMENTS.  IF A DASH OR ANY  BUG01          
*                CHARACTER WITH A  VALUE LESS THAN 'A' WAS       BUG01          
*                ENCOUNTERED, END  OF ELEMENT WAS PROCESSED.     BUG01          
*                                                                BUG01          
* ROSA  6/20/89  INCREASE SIZE OF JOB NUMBER TABLE FROM 120,000 TO L01          
*                200,000                                           L01          
*                ALSO ADD LOGIC TO USE ESTIMATE TO SEARCH THRU     L01          
*                JOB TABLE.. IF MATCH DO NOT DELETE  JOB           L01          
         TITLE 'PP0102 - PRINTPAK PURGE PROGRAM'                                
*                                                                               
*        QOPT1   RECORD TYPE H=CLIENTS                                          
*                            R=REPS                                             
*                            J=JOBS                                             
*                            C=COMMENTS                                         
*                            P=PUBS                                             
*                            L=PUB LIST                                         
*                            S=CLEARANCE STATUS RECORDS                         
*                               NOTE: FOR ONE CLIENT ONLY                       
*        QOPT2  PURGE OPTION A=TRY TO PURGE ALL UNUSED RECORDS                  
*                            BLANK= TRY TO PURGE ONLY THOSE RECORDS             
*                            MARKED *PURGE                                      
*                          B= PURGE ONLY THOSE RECORDS MARKED "*PURGE"          
*                             OR "PURGE"                                        
*                            G= PURGE ONLY PURGEABLE RECORDS                    
*                                                                               
*        QOPT3               P= PRODUCE 11ZZ CARDS (PUB PURGES)                 
*                            C=PREVENT PUB PURGE IF CONTRACTS                   
*                              FOUND WHEN PURGING PUBS (QOPT1=P)                
*                                                                               
*        QOPT4   KEY OPTION  Y= PRINT KEYS OF RECORDS USING AND                 
*                               PRUGED/NOT PURGED MESSAGES                      
*                               EVEN IF QOPT2 = A                               
*                                                                               
*        QOPT5  FOR CLIENT OR PUB PURGES   Y= PURGE CLIENT'S RECORDS            
*               OR PUB LIST PURGES          OR PURGE RECORDS USING PUB          
*                                 OR CLEAR PUB LIST CODE FROM JOB RECS          
*                                                                               
*        QOPT6  FOR COMMENT PURGE C= TO DELETE COMMENT ELEMS                    
*               FROM CONTRACTS ONLY J= TO DELETE COMMENT ELEMS                  
*               FROM JOB RECORDS ONLY P= (PRODUCT), E= (ESTIMATE),              
*               B= (BUY), R= (AOR), U= (USERP), L= (PUBLICATION)                
*               AND A= FROM ALL OF THE ABOVE RECORDS                            
*                                                                               
*        QOPT7  T=RCWRITE IS NO                                                 
*                                                                               
*********                                                                       
*********       NOTE - USE EXTREME CARE IF USING THIS OPTION WITH               
*********       ALL CLIENT AND ALL COMMENT REQUESTS (QOPT2 = A)                 
*********                                                                       
*                                                                               
*  ****************  FOR ONE RECORD PURGES  ********************                
*                                                                               
*  CLIENTS   (H) - QCLIENT NOT EQUAL TO SPACES OR "ALL"                         
*  PUBS      (P) - QPUB NOT EQUAL TO SPACE                                      
*  REPS      (R) - QPUB+1=R   QPUB+2=REP CODE                                   
*  COMMENTS  (C) - QPUB+1=C   QPUB+2=COMMENT NUMBER                             
*  JOBS      (J) - QPUB+1=J   QPUB+2=JOB CODE                                   
*                  (QCLIENT AND QPRODUCT MUST ALSO BE ENTERED)                  
*  PUB LISTS (L) - QPUB+1=L   QPUB+2=LIST CODE                                  
*                  (QCLIENT MUST ALSO BE ENTERED)                               
*                                                                               
***********************************************************************         
*                                                                               
PP0102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP0102                                                         
*                                                                               
         LA    R7,1(RB)                                                         
         LA    R7,4095(R7)                                                      
         USING PP0102+4096,R7                                                   
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP01WRKD,R8                                                      
*                                                                               
         MVC   PKFLAG,SPACES       FLAG USED TO CONTROL PRTKEY HEADERS          
         MVC   RSFLAG,SPACES       FLAG USED TO CONTROL RESULT HEADERS          
*                                                                               
         CLI   MODE,PROCREQ                                                     
         BNE   EXIT                                                             
         RELOC (R3)                                                             
         L     RF,=A(BSTAB)                                                     
         AR    RF,R3                                                            
         ST    RF,ABSTAB                                                        
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,X'FD'                                                   
*                                                                               
         ZAP   PRGCNT,=P'0'                                                     
         ZAP   NPRGCNT,=P'0'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   QOPT7,C'T'                                                       
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'        SET TO NO IF QOPT7=T                         
*                                                                               
         BAS   RE,CHKONE        SEE IF ONE RECORD REQUESTED                     
*                               IF SO SET QOPT2 TO "P"                          
*                               SO THAT IT'S USES ALWAYS GET REPORTED           
         CLI   QOPT1,C'C'                                                       
         BE    COMMENTS                                                         
         CLI   QOPT1,C'J'                                                       
         BE    JOBS                                                             
         CLI   QOPT1,C'R'                                                       
         BE    REPS                                                             
         CLI   QOPT1,C'H'                                                       
         BE    HDRS                CLIENT HDRS                                  
         CLI   QOPT1,C'S'     CLEARANCE STATUS RECORDS PURGE ONLY ?             
         BE    HDRS                                                             
         CLI   QOPT1,C'L'                                                       
         BE    LSTS                PUB LIST RECS                                
*                                                                               
         CLI   QOPT1,C'P'          PUB PURGE ?                                  
         BNE   EXIT                                                             
         GOTO1 =A(PUBS)                                                         
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
CHKONE   DS    0H                                                               
         CLC   QPUB,SPACES    CHECKS MOST ONE RECORD REQUESTS                   
         BNE   CHKONEP        MUST BE ONE RECORD                                
         CLI   QOPT1,C'H'     SEE IF PURGING CLIENTS                            
         BNE   CHKONEX        NO - THE LEAVE QOPT2 ALONE                        
         CLC   QCLIENT,SPACES                                                   
         BE    CHKONEX                                                          
         CLC   QCLIENT,=C'ALL'                                                  
         BE    CHKONEX                                                          
*                                                                               
CHKONEP  MVI   QOPT2,C'P'                                                       
CHKONEX  BR    RE              RETURN                                           
*                                                                               
         EJECT                                                                  
*                             REPS                                              
         SPACE 2                                                                
REPS     DS    0H                                                               
         MVI   RCSUBPRG,20                                                      
*                                  SET BSPARS                                   
         XC    BSPARS,BSPARS                                                    
         L     RF,ABSTAB                                                        
         ST    RF,BSPARS+4                                                      
         LA    RF,6                LENGHT OF REC                                
         ST    RF,BSPARS+12                                                     
         LA    RF,5                LENGTH OF KEY                                
*                                  REP CODE PLUS SUFFIX                         
         ST    RF,BSPARS+16                                                     
         L     RF,=A(BSTAB6)                                     L01            
         ST    RF,BSPARS+20                                                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'11'                                                      
         CLI   QPUB+1,C'R'         ONE REP ONLY ?                               
         BNE   *+10                NO                                           
         MVC   KEY+4(4),QPUB+2                                                  
         GOTO1 HIGH                                                             
         B     REPS2B                                                           
REPS2    DS    0H                                                               
         GOTO1 SEQ                                                              
REPS2B   DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   REPS4                                                            
         CLI   QPUB+1,C'R'         ONE REP ONLY ?                               
         BNE   REPS2D              NO                                           
         CLC   KEY+4(4),QPUB+2     YES                                          
         BNE   REPS4                                                            
         B     REPS3                                                            
REPS2D   CLI   QOPT2,C'A'                                                       
         BE    REPS3                                                            
         CLI   QOPT2,C'G'       PURGEABLE ONLY?                                 
         BE    REPS3                                                            
         LA    RF,PREPREC                                                       
         ST    RF,AREC                                                          
         GOTO1 GETPRT                                                           
         CLC   PREPNAME(6),=C'*PURGE'                                           
         BE    REPS3                                                            
         CLC   PREPNAME(7),=C'**PURGE'                                          
         BE    REPS3                                                            
         CLC   PREPNAME(8),=C'***PURGE'                                         
         BE    REPS3                                                            
*                                                                               
         CLI   QOPT2,C'B'        SEE IF ALSO CHECKING FOR "PURGE"               
         BNE   REPS2                                                            
         CLC   PREPNAME(5),=C'PURGE '                                           
         BE    REPS3                                                            
         B     REPS2                                                            
*                                                                               
REPS3    DS    0H                                                               
         XC    DOUBLE,DOUBLE                                                    
         MVC   DOUBLE(5),KEY+4 ,   REP KEYS MIGHT HAVE 5                        
         GOTO1 BINSRCH,BSPARS,(1,DOUBLE)                                        
*                                                                               
         OC    BSPARS+1(3),BSPARS+1                                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         B     REPS2                                                            
*                                                                               
REPS4    DS    0H                                                               
         OC    BSPARS+8(4),BSPARS+8                                             
         BZ    EXIT                                                             
         XC    PUBREC(33),PUBREC                                                
         XC    LASTPUB,LASTPUB                                                  
REPS4B   DS    0H                                                               
         BAS   RE,NXTPUB                                                        
         CLI   PUBREC,X'FF'                                                     
         BE    REPSE         END OF PUBS - CONTINUE CHECKS                      
         MVI   PUBLSW,0                                                         
*                                      FIRST CHECK FOR PUBLISHER                
         OC    PUBPLSH,PUBPLSH                                                  
         BZ    REPS5                   NO PUBLISHER                             
         MVC   DOUBLE(4),PUBPLSH                                                
         MVI   DOUBLE+4,0                                                       
         MVI   DOUBLE+5,0                                                       
         GOTO1 BINSRCH,BSPARS,DOUBLE                                            
         CLI   BSPARS,1                                                         
         BE    REPS5                                                            
         L     RF,BSPARS                                                        
         MVI   5(RF),X'FF'             REP USED AS PUBLISHER                    
*                                                                               
         MVI   PUBLSW,C'Y'                                                      
         GOTO1 =A(PRNTKEY)                                                      
         MVI   PUBLSW,0                                                         
*                                                                               
REPS5    LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'14'                                                     
         CLI   0(R2),X'14'                                                      
         BE    REPS6B                                                           
REPS6    DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   REPS4B                                                           
REPS6B   DS    0H                                                               
         USING PUBREPEL,R2                                                      
         LA    R3,PUBPAREP                                                      
         LA    R4,3                                                             
REPS7    DS    0H                                                               
         CLI   QMEDIA,C'O'         SEE IF DOING OUTDOOR                         
         BNE   REPS7C                                                           
         CHI   R4,2                SEE IF DOING TRAFFIC REP                     
         BNE   REPS7C              NO                                           
         CLC   0(3,R3),=C'000'     SEE IF DOING SUFFIX                          
         BNE   REPS7C                                                           
         MVC   DOUBLE(4),PUBCNREP  USE CONTRACT AS BASE REP                     
         MVC   DOUBLE+4(1),3(R3)   SUFFIX                                       
         B     REPS7C5                                                          
*                                                                               
REPS7C   MVC   DOUBLE(4),0(R3)                                                  
         MVI   DOUBLE+4,0                                                       
REPS7C5  MVI   DOUBLE+5,0                                                       
         GOTO1 BINSRCH,BSPARS,DOUBLE                                            
*                                                                               
         CLI   BSPARS,1                                                         
         BE    REPS7D                                                           
         L     RF,BSPARS                                                        
         MVI   5(RF),X'FF'                                                      
         GOTO1 =A(PRNTKEY)                                                      
         B     REPS7F                                                           
*                                                                               
REPS7D   DS    0H                                                               
         OC    DOUBLE(5),DOUBLE                                                 
         BZ    REPS7F                                                           
         CLI   QOPT2,C'G'        PURGEABLE ONLY?                                
         BE    REPS7E                                                           
         CLI   QOPT2,C'A'        SEE IF DOING ALL REPS                          
         BNE   REPS7F                                                           
REPS7E   MVI   MISSSW,C'Y'        REP NOT ON FILE                               
         GOTO1 =A(PRNTKEY)                                                      
         MVI   MISSSW,C'N'                                                      
*                                                                               
REPS7F   LA    R3,4(R3)                                                         
         BCT   R4,REPS7                                                         
         B     REPS6                                                            
*                                  NOW CHECK SPECIAL REP ON ESTIMATES           
REPSE    DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'07'         ESTIMATES                                    
*                                                                               
         GOTO1 HIGH                                                             
         B     REPSE2B                                                          
REPSE2   DS    0H                                                               
         GOTO1 SEQ                                                              
REPSE2B  DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   REPSJ               I CK FOR BUYREC USES OF REP                  
                                                                                
         DS    0H                                                               
         LA    RF,PESTREC                                                       
         ST    RF,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         DS    0H                                                               
         XC    DOUBLE(5),DOUBLE                                                 
         OC    PESTREP,PESTREP         ESTIMATE SPECIAL REP                     
         BZ    REPSE2                                                           
         MVC   DOUBLE(4),PESTREP                                                
         MVI   DOUBLE+4,0                                                       
*                                                                               
         GOTO1 BINSRCH,BSPARS,DOUBLE                                            
         CLI   BSPARS,1            RECORD NOT IN TABLE                          
         BE    REPSE2                                                           
         L     RF,BSPARS                                                        
         MVI   5(RF),X'FF'                                                      
         GOTO1 =A(PRNTKEY)                                                      
         B     REPSE2        KEEP LOOKING                                       
*                                                                               
REPSJ    DS    0H          CHECK PRODUCTION HOUSE AND BILLING REPS              
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'15'         JOB REC                                      
*                                                                               
         GOTO1 HIGH                                                             
         B     REPSJ2B                                                          
REPSJ2   DS    0H                                                               
         GOTO1 SEQ                                                              
REPSJ2B  DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   REPSB               I CK FOR BUYREC USES OF REP                  
                                                                                
         DS    0H                                                               
         LA    RF,PJOBREC                                                       
         ST    RF,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         DS    0H                                                               
         XC    DOUBLE(5),DOUBLE                                                 
         OC    PJOBPROD,PJOBPROD                                                
         BZ    REPSJ3A                                                          
         MVC   DOUBLE(4),PJOBPROD                                               
         MVI   DOUBLE+4,0                                                       
*                                                                               
         GOTO1 BINSRCH,BSPARS,DOUBLE                                            
         CLI   BSPARS,1            RECRD NOT FD                                 
         BE    REPSJ2                                                           
         L     RF,BSPARS                                                        
         MVI   5(RF),X'FF'                                                      
         GOTO1 =A(PRNTKEY)                                                      
*                                                                               
REPSJ3A  OC    PJOBBREP,PJOBBREP                                                
         BZ    REPSJ2                                                           
         MVC   DOUBLE(4),PJOBBREP                                               
         MVI   DOUBLE+4,0                                                       
*                                                                               
         GOTO1 BINSRCH,BSPARS,DOUBLE                                            
         CLI   BSPARS,1            RECRD NOT FD                                 
         BE    REPSJ2                                                           
         L     RF,BSPARS                                                        
         MVI   5(RF),X'FF'                                                      
         GOTO1 =A(PRNTKEY)                                                      
         B     REPSJ2                                                           
*                                                                               
REPSB    DS    0H                                                               
         OC    BSPARS+1(3),BSPARS+1                                             
         BZ    EXIT                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'20'          BUYREC                                      
         GOTO1 HIGH                                                             
         B     REPSB2                                                           
REPSB1   DS    0H                                                               
         GOTO1 SEQ                                                              
REPSB2   DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   REPS9                                                            
         LA    RF,PBUYREC                                                       
         ST    RF,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         MVI   ELCODE,X'80'        PBSREPEL                                     
         LA    R2,PBUYREC+33                                                    
         BAS   RE,NEXTEL                                                        
         BNE   REPSB1                                                           
*                                                                               
         OC    2(4,R2),2(R2)       PBSREP                                       
         BZ    REPSB1                                                           
         MVC   DOUBLE(4),2(R2)                                                  
         MVI   DOUBLE+4,0                                                       
         GOTO1 BINSRCH,BSPARS,DOUBLE                                            
*                                                                               
         CLI   BSPARS,1            RECORD NOT FOUND                             
         BE    REPSB1                                                           
         L     RF,BSPARS                                                        
         MVI   5(RF),X'FF'                                                      
*                                                                               
         GOTO1 =A(PRNTKEY)                                                      
         B     REPSB1                                                           
*                                                                               
REPS9    DS    0H                                                               
         GOTO1 =A(RESULTS)                                                      
         B     EXIT                                                             
         EJECT                                                                  
*                             JOB RECORDS                                       
         DROP  R2                                                               
         SPACE 2                                                                
JOBS     DS    0H                                                               
         MVI   RCSUBPRG,30                                                      
*                             SET BSPARS                                        
         XC    BSPARS,BSPARS                                                    
         L     RF,ABSTAB                                                        
         ST    RF,BSPARS+4                                                      
         LA    RF,13          L'RECORD                                          
         ST    RF,BSPARS+12                                                     
         LA    RF,12                                                            
         ST    RF,BSPARS+16                                                     
         L     RF,=A(BSTAB13)                                    L01            
         ST    RF,BSPARS+20                                                     
*                                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'15'                                                      
         CLI   QPUB+1,C'J'         ONE JOB ONLY ?                               
         BNE   JOBSA               NO                                           
         MVC   KEY+4(3),QCLIENT    YES - SET CLT                                
         MVC   KEY+7(3),QPRODUCT         PRODUCT                                
         MVC   KEY+10(6),QPUB+2          & JOB                                  
         MVC   WORK(12),KEY+4      SAVE FOR TEST IN JOBS2B                      
         B     JOBS1                                                            
JOBSA    CLC   QCLIENT,=C'ALL'                                                  
         BE    JOBS1                                                            
         CLC   QCLIENT,=C'   '                                                  
         BE    JOBS1                                                            
         MVC   KEY+4(3),QCLIENT                                                 
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    JOBS1                                                            
         CLC   QPRODUCT,=C'   '                                                 
         BE    JOBS1                                                            
         MVC   KEY+7(3),QPRODUCT      ONE PRODUCT                               
*                                                                               
JOBS1    GOTO1 HIGH                                                             
         B     JOBS2B                                                           
JOBS2    DS    0H                                                               
         GOTO1 SEQ                                                              
JOBS2B   DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   JOBS4                                                            
         CLI   QPUB+1,C'J'         ONE JOB ONLY ?                               
         BNE   JOBS2C              NO                                           
         CLC   KEY+4(12),WORK      YES                                          
         BNE   JOBS4                                                            
         B     JOBS3                                                            
JOBS2C   CLC   QCLIENT,=C'ALL'                                                  
         BE    JOBS2D                                                           
         CLC   QCLIENT,=C'   '                                                  
         BE    JOBS2D                                                           
         CLC   KEY(7),KEYSAVE                                                   
         BNE   JOBS4                                                            
*                                                                               
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    JOBS2D                                                           
         CLC   QPRODUCT,=C'   '                                                 
         BE    JOBS2D                                                           
         CLC   KEY(10),KEYSAVE       ONE PRODUCT                                
         BNE   JOBS4                                                            
*                                                                               
JOBS2D   CLI   QOPT2,C'A'                                                       
         BE    JOBS3                                                            
         CLI   QOPT2,C'G'        PURGEABLE ONLY?                                
         BE    JOBS3                                                            
         LA    RF,PJOBREC                                                       
         ST    RF,AREC                                                          
         GOTO1 GETPRT                                                           
         CLC   PJOBCAP1(6),=C'*PURGE'                                           
         BE    JOBS3                                                            
         CLC   PJOBCAP1(7),=C'**PURGE'                                          
         BE    JOBS3                                                            
         CLC   PJOBCAP1(8),=C'***PURGE'                                         
         BE    JOBS3                                                            
*                                                                               
         CLI   QOPT2,C'B'        SEE IF ALSO CHECKING FOR "PURGE"               
         BNE   JOBS2                                                            
         CLC   PJOBCAP1(5),=C'PURGE '                                           
         BE    JOBS3                                                            
         B     JOBS2                                                            
*                                                                               
JOBS3    DS    0H                                                               
         XC    WORK(13),WORK                                                    
         MVC   WORK(12),KEY+4                                                   
         GOTO1 BINSRCH,BSPARS,(1,WORK)                                          
*                                                                               
         OC    BSPARS+1(3),BSPARS+1                                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         B     JOBS2                                                            
JOBS4    DS    0H                                                               
         OC    BSPARS+8(4),BSPARS+8                                             
         BZ    EXIT                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'20'                                                      
         GOTO1 HIGH                                                             
         B     JOBS6B                                                           
JOBS6    DS    0H                                                               
         GOTO1 SEQ                                                              
JOBS6B   DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   JOBS10                                                           
         LA    RF,PBUYREC                                                       
         ST    RF,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         MVC   WORK(6),PBUYKCLT                                                 
         MVC   WORK+6(6),PBDJOB                                                 
         MVI   WORK+12,0                                                        
         GOTO1 BINSRCH,BSPARS,WORK                                              
*                                                                               
         CLI   BSPARS,1                                                         
         BE    JOBS6                                                            
         L     RF,BSPARS                                                        
         MVI   12(RF),X'FF'                                                     
JOBS7    DS    0H                                                               
         GOTO1 =A(PRNTKEY)                                                      
         B     JOBS6                                                            
*                                                                               
*                                                                               
JOBS10   DS    0H USE ESTIMATES TO FILTER THRU TABLE                            
         XC    KEY,KEY                                          L01             
         MVC   KEY(3),PAGYKAGY L01                                              
         MVI   KEY+3,X'07'                                                      
         GOTO1 HIGH                                                             
         B     JOBS6BE                                                          
JOBS6E   DS    0H                                                               
         GOTO1 SEQ                                                              
JOBS6BE  DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   JOBS10E                                                          
         LA    RF,PESTREC                                                       
         ST    RF,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         MVC   WORK(6),PESTKCLT                                                 
         OC    PESTJOB,PESTJOB                                                  
         BZ    JOBS6E                                                           
         MVC   WORK+6(6),PESTJOB                                                
         MVI   WORK+12,0                                                        
         GOTO1 BINSRCH,BSPARS,WORK                                              
*                                                                               
         CLI   BSPARS,1    JOB IN EST NOT IN TABLE                              
         BE    JOBS6E                                                           
         L     RF,BSPARS                                                        
         MVI   12(RF),X'FF'                                                     
JOBS7E   DS    0H                                                               
         GOTO1 =A(PRNTKEY)                                                      
         B     JOBS6E                                                           
*                                                                               
JOBS10E  GOTO1 =A(RESULTS)                                                      
         B     EXIT                                                             
         EJECT                                                                  
*                             COMMENTS                                          
COMMENTS DS    0H                                                               
         MVI   RCSUBPRG,40                                                      
*                                  SET BSPARS                                   
         XC    BSPARS,BSPARS                                                    
         L     RF,ABSTAB                                                        
         ST    RF,BSPARS+4                                                      
         LA    RF,7                L'REC                                        
         ST    RF,BSPARS+12                                                     
         LA    RF,6                                                             
         ST    RF,BSPARS+16                                                     
         L     RF,=A(BSTAB7)                                    L01             
         ST    RF,BSPARS+20                                                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'40'                                                      
         CLI   QPUB+1,C'C'         ONE COMMENT ONLY ?                           
         BNE   *+10                NO                                           
         MVC   KEY+4(6),QPUB+2                                                  
         GOTO1 HIGH                                                             
         B     CM2B                                                             
CM2      DS    0H                                                               
         GOTO1 SEQ                                                              
CM2B     DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   CM4                                                              
         CLI   QPUB+1,C'C'         ONE COMMENT ONLY ?                           
         BNE   CM2D                NO                                           
         CLC   KEY+4(6),QPUB+2     YES                                          
         BNE   CM4                                                              
         B     CM3                                                              
CM2D     L     RF,ALISREC                                                       
         ST    RF,AREC                                                          
         CLI   QOPT2,C'A'                                                       
         BE    CM3                                                              
         CLI   QOPT2,C'G'          PURGEABLE ONLY?                              
         BE    CM3                                                              
         GOTO1 GETPRT                                                           
         L     RF,ALISREC                                                       
         CLC   35(6,RF),=C'*PURGE'                                              
         BE    CM3                                                              
         CLC   35(7,RF),=C'**PURGE'                                             
         BE    CM3                                                              
         CLC   35(8,RF),=C'***PURGE'                                            
         BE    CM3                                                              
*                                                                               
         CLI   QOPT2,C'B'        SEE IF ALSO CHECKING FOR "PURGE"               
         BNE   CM2                                                              
         CLC   35(5,RF),=C'PURGE '                                              
         BE    CM3                                                              
         B     CM2                                                              
*                                                                               
*                                                                               
CM3      DS    0H                                                               
         XC    DOUBLE,DOUBLE                                                    
         MVC   DOUBLE(6),KEY+4                                                  
         GOTO1 BINSRCH,BSPARS,(1,DOUBLE)                                        
*                                                                               
         OC    BSPARS+1(3),BSPARS+1                                             
         BNZ   CM2                                                              
         DC    H'0'                                                             
*                                                                               
CM4      DS    0H                                                               
         OC    BSPARS+8(4),BSPARS+8                                             
         BZ    EXIT                NO COMMENTS                                  
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'10'         CONTRACTS                                    
         GOTO1 HIGH                                                             
         B     CM6B                                                             
CM6      DS    0H                                                               
         GOTO1 SEQ                                                              
CM6B     DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   CM7                                                              
         MVI   WRTREC,0                                                         
         LA    RF,PCONREC                                                       
         ST    RF,AREC                                                          
         GOTO1 GETPRT                                                           
CM6C     LA    R2,PCONREC+33                                                    
         MVI   ELCODE,X'30'                                                     
         CLI   0(R2),X'30'                                                      
         BE    CM6D5                                                            
CM6D     BAS   RE,NEXTEL                                                        
         BE    CM6D5                                                            
         CLI   WRTREC,C'Y'   SEE IF I MUST WRITE BACK THE CONTRACT              
         BNE   CM6                                                              
         GOTO1 PUTPRT                                                           
         MVI   WRTREC,0          TURN OFF WRITE BACK FLAG                       
         B     CM6                                                              
*                                                                               
CM6D5    MVC   DOUBLE(6),2(R2)                                                  
         MVI   DOUBLE+6,0                                                       
         GOTO1 BINSRCH,BSPARS,DOUBLE                                            
*                                                                               
         CLI   BSPARS,1                                                         
         BE    CM6E                                                             
         L     RF,BSPARS                                                        
         MVI   6(RF),X'FF'         DO NOT PURGE STANDARD COMMENT REC            
         CLI   QOPT6,C'C'     SEE IF DELETING CONTRACT COMMENTS                 
         BE    CM6D20                                                           
         CLI   QOPT6,C'A'     SEE IF DEL ALL  COMMENTS                          
         BNE   CM6F                                                             
         MVI   6(RF),0             OK TO PURGE STANDARD COMMENT REC             
CM6D20   BAS   RE,COMDEL                                                        
         MVC   PSECOND+19(25),=C'*COMMENT ELEMENT DELETED*'                     
         B     CM6F                                                             
*                                                                               
CM6E     DS    0H                                                               
         CLI   QOPT2,C'G'          PURGEABLE ONLY?                              
         BE    CM6E2                                                            
         CLI   QOPT2,C'A'           SEE IF DOING ALL COMMENTS                   
         BNE   CM6D                 NO CHECK NEXT ELEM                          
CM6E2    DS    0H                                                               
         OC    DOUBLE(6),DOUBLE                                                 
         BZ    CM6D                 CHECK NEXT ELEM                             
         CLI   QOPT6,C'A'     SEE IF DEL ALL COMMENTS                           
         BE    *+12                                                             
         CLI   QOPT6,C'C'     SEE IF DELETING CONTRACT COMMENTS                 
         BNE   CM6E5                                                            
         BAS   RE,COMDEL                                                        
         MVC   PSECOND+19(25),=C'*COMMENT ELEMENT DELETED*'                     
*                                                                               
CM6E5    MVI   MISSSW,C'Y'                                                      
         GOTO1 =A(PRNTKEY)                                                      
         MVI   MISSSW,C'N'                                                      
         B     CM6D                                                             
*                                                                               
CM6F     DS    0H                                                               
         GOTO1 =A(PRNTKEY)                                                      
         B     CM6D                                                             
*                                                                               
CM7      DS    0H                  *** AOR RECORD ***                           
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'14'         AOR                                          
         GOTO1 HIGH                                                             
         B     CM7B                                                             
CM7A     DS    0H                                                               
         GOTO1 SEQ                                                              
CM7B     DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   CM8                                                              
         MVI   WRTREC,0                                                         
         LA    RF,PCONREC                                                       
         ST    RF,AREC                                                          
         GOTO1 GETPRT                                                           
CM7C     LA    R2,PCONREC+33                                                    
         MVI   ELCODE,X'66'        STANDARD COMMENT ELEM                        
         CLI   0(R2),X'66'                                                      
         BE    CM7D5                                                            
CM7D     BAS   RE,NEXTEL                                                        
         BE    CM7D5                                                            
         CLI   WRTREC,C'Y'   SEE IF I MUST WRITE BACK THE AOR REC               
         BNE   CM7A                                                             
         GOTO1 PUTPRT                                                           
         MVI   WRTREC,0          TURN OFF WRITE BACK FLAG                       
         B     CM7A                                                             
*                                                                               
CM7D5    MVC   DOUBLE(6),2(R2)                                                  
         MVI   DOUBLE+6,0                                                       
         GOTO1 BINSRCH,BSPARS,DOUBLE                                            
*                                                                               
         CLI   BSPARS,1                                                         
         BE    CM7E                                                             
         L     RF,BSPARS                                                        
         MVI   6(RF),X'FF'         DO NOT PURGE STANDARD COMMENT REC            
         CLI   QOPT6,C'R'     SEE IF DELETING AOR COMMENTS                      
         BE    CM7D20                                                           
         CLI   QOPT6,C'A'     SEE IF DEL ALL  COMMENTS                          
         BNE   CM7F                                                             
         MVI   6(RF),0             OK TO PURGE STANDARD COMMENT REC             
CM7D20   BAS   RE,COMDEL                                                        
         MVC   PSECOND+19(25),=C'*COMMENT ELEMENT DELETED*'                     
         B     CM7F                                                             
*                                                                               
CM7E     DS    0H                                                               
         CLI   QOPT2,C'G'          PURGEABLE ONLY?                              
         BE    CM7E2                                                            
         CLI   QOPT2,C'A'           SEE IF DOING ALL COMMENTS                   
         BNE   CM7D                 NO - CHECK NEXT ELEM                        
CM7E2    DS    0H                                                               
         OC    DOUBLE(6),DOUBLE                                                 
         BZ    CM7D                 CHECK NEXT ELEM                             
         CLI   QOPT6,C'A'     SEE IF DEL ALL COMMENTS                           
         BE    *+12                                                             
         CLI   QOPT6,C'R'     SEE IF DELETING AOR COMMENTS                      
         BNE   CM7E5                                                            
         BAS   RE,COMDEL                                                        
         MVC   PSECOND+19(25),=C'*COMMENT ELEMENT DELETED*'                     
*                                                                               
CM7E5    MVI   MISSSW,C'Y'                                                      
         GOTO1 =A(PRNTKEY)                                                      
         MVI   MISSSW,C'N'                                                      
         B     CM7D                                                             
*                                                                               
CM7F     DS    0H                                                               
         GOTO1 =A(PRNTKEY)                                                      
         B     CM7D                                                             
*                                                                               
CM8      DS    0H                  CLIENT HEADERS                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'02'         CLIENTS                                      
         GOTO1 HIGH                                                             
         B     CM8B                                                             
CM8A     DS    0H                                                               
         GOTO1 SEQ                                                              
CM8B     DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   CM12                                                             
         LA    RF,PCLTREC                                                       
         ST    RF,AREC                                                          
         GOTO1 GETPRT                                                           
         LA    R2,PCLTREC+33                                                    
         MVI   ELCODE,X'10'    FIRST CHECK FOR CONTRACT STD COMM                
CM8D     BAS   RE,NEXTEL                                                        
         BNE   CM8D5                                                            
*                                                                               
         MVC   DOUBLE(6),2(R2)                                                  
         MVI   DOUBLE+6,0                                                       
         GOTO1 BINSRCH,BSPARS,DOUBLE                                            
*                                                                               
         CLI   BSPARS,1                                                         
         BE    CM8E                                                             
         L     RF,BSPARS                                                        
         MVI   6(RF),X'FF'                                                      
         B     CM8F                                                             
*                                                                               
CM8D5    LA    R2,PCLTREC+33                                                    
         MVI   ELCODE,X'11'    THEN CHECK FOR I/O STD COMM                      
CM8D8    BAS   RE,NEXTEL                                                        
         BNE   CM8A             GO CHECK NEXT CLIENT                            
*                                                                               
         MVC   DOUBLE(6),2(R2)                                                  
         MVI   DOUBLE+6,0                                                       
         GOTO1 BINSRCH,BSPARS,DOUBLE                                            
*                                                                               
         CLI   BSPARS,1                                                         
         BE    CM8E                                                             
         L     RF,BSPARS                                                        
         MVI   6(RF),X'FF'                                                      
         B     CM8F                                                             
*                                                                               
CM8E     DS    0H                                                               
         CLI   QOPT2,C'G'          PURGEABLE ONLY?                              
         BE    CM8E2                                                            
         CLI   QOPT2,C'A'          SEE IF DOING ALL COMMENTS                    
         BNE   CM8X                                                             
CM8E2    DS    0H                                                               
         OC    DOUBLE(6),DOUBLE                                                 
         BZ    CM8X                                                             
         MVI   MISSSW,C'Y'                                                      
         GOTO1 =A(PRNTKEY)                                                      
         MVI   MISSSW,C'N'                                                      
         B     CM8X                                                             
*                                                                               
CM8F     DS    0H                                                               
         GOTO1 =A(PRNTKEY)                                                      
         B     CM8X                                                             
*                                                                               
CM8X     CLI   ELCODE,X'10'    SEE IF SEARCHING FOR CONTRACT COMM               
         BE    CM8D            YES                                              
         B     CM8D8           NO - MUST HAVE BEEN I/O COMM                     
*                                  JOB RECORDS                                  
CM12     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'15'                                                      
         LA    R0,PJOBREC                                                       
         ST    R0,AREC                                                          
         MVI   ELCODE,X'66'                                                     
CM12C    GOTO1 HIGH                                                             
         B     CM14B                                                            
CM14     DS    0H                                                               
         CLI   WRTREC,C'Y'       SEE IF MUST WRITE BACK RECORD                  
         BNE   CM14A                                                            
         GOTO1 PUTPRT                                                           
         MVI   WRTREC,0                                                         
CM14A    GOTO1 SEQ                                                              
CM14B    DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   CM25                                                             
         OC    KEY+16(6),KEY+16                                                 
         BZ    CM14A                                                            
         GOTO1 GETPRT                                                           
CM15     L     R2,AREC                                                          
         LA    R2,33(R2)                                                        
         CLC   0(1,R2),ELCODE                                                   
         BE    CM16B                                                            
CM16     CLI   KEY+3,X'15'         SEE IF DOING A JOB REC                       
         BE    CM16A                                                            
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   CM25D                TRY NEXT ELCODE                             
         B     CM16B                                                            
*                                                                               
CM16A    BAS   RE,NEXTEL                                                        
         BNE   CM14                DO NEXT RECORD                               
CM16B    DS    0H                                                               
         SR    R6,R6                                                            
         IC    R6,1(R2)                                                         
         LA    R6,0(R2,R6)         END OF COMMENT                               
         LA    R3,2(R2)                                                         
CM18     DS    0H                                                               
         CLC   0(4,R3),=C'COM='                                                 
         BNE   CM16                                                             
         LA    R3,4(R3)                                                         
         LR    R4,R3                                                            
CM20     DS    0H                                                               
*        CLI   0(R3),C'A'                                       BUG01           
*        BL    CM22                                             BUG01           
         CLC   0(1,R3),ELCODE                                   BUG01           
         BE    CM22                                             BUG01           
         CLI   0(R3),C','                                       BUG01           
         BE    CM22                                             BUG01           
         CR    R6,R3               END OF ELEM                  BUG01           
         BE    CM22                                             BUG01           
         LA    R3,1(R3)                                                         
         B     CM20                                                             
*                                                                               
CM22     DS    0H                                                               
         LR    R5,R4                                                            
         SR    R4,R3                                                            
         LCR   R4,R4                                                            
         BNP   CM24                                                             
         CHI   R4,6                                                             
         BNH   *+8                                                              
         LA    R4,6                                                             
         MVC   DUB,SPACES                                                       
         LA    R1,DUB+6                                                         
         SR    R1,R4                                                            
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R5)                                                    
*                                                                               
         MVC   MYDUB,SPACES                                                     
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   MYDUB,0(R5)                                                      
*                                                                               
         MVI   DUB+6,0                                                          
         GOTO1 BINSRCH,BSPARS,DUB                                               
*                                                                               
         CLI   BSPARS,1                                                         
         BE    CM22E                                                            
         L     RF,BSPARS                                                        
         MVI   6(RF),X'FF'         DO NOT PURGE STANDARD COMMENT REC            
         CLI   KEY+3,X'15'         SEE IF DOING JOB RECORD                      
         BNE   CM22C                                                            
         CLI   QOPT6,C'J'          SEE IF DOING JOB COMMENT                     
         BE    CM22B                                                            
         CLI   QOPT6,C'A'     SEE IF DEL ALL COMMENTS                           
         BNE   CM22D                                                            
         MVI   6(RF),0             OK TO PURGE STANDARD COMMENT REC             
CM22B    BAS   RE,COMDELJ                                                       
         CLI   FOUND,C'N'                                                       
         BE    *+10                                                             
         MVC   PSECOND+19(29),=C'*COMMENT ELEMENT (J) DELETED*'                 
         B     CM22D                                                            
*                                                                               
CM22C    CLI   QOPT6,C'B'          BUY COMMENTS                                 
         BE    CM22C20                                                          
         CLI   QOPT6,C'A'          ALL COMMENTS                                 
         BNE   CM22D                                                            
         MVI   6(RF),0             OK TO PURGE STANDARD COMMENT REC             
CM22C20  BAS   RE,COMDELB                                                       
         MVC   PSECOND+19(29),=C'*COMMENT ELEMENT (B) DELETED*'                 
*                                                                               
CM22D    GOTO1 =A(PRNTKEY)                                                      
         B     CM24                                                             
*                                                                               
CM22E    DS    0H                                                               
         CLI   QOPT2,C'G'          PURGEABLE ONLY?                              
         BE    CM22E2                                                           
         CLI   QOPT2,C'A'         SEE IF DOING ALL COMMENTS                     
         BNE   CM24                                                             
*                                                                               
CM22E2   DS    0H                                                               
         OC    DUB(6),DUB                                                       
         BZ    CM24                                                             
         CLI   KEY+3,X'15'         SEE IF DOING JOB RECORD                      
         BNE   CM22F1                                                           
         CLI   QOPT6,C'A'     SEE IF DEL ALL COMMENTS                           
         BE    *+12                                                             
         CLI   QOPT6,C'J'          SEE IF DOING JOB COMMENT                     
         BNE   CM22F                                                            
         BAS   RE,COMDELJ                                                       
         CLI   FOUND,C'N'                                                       
         BE    *+10                                                             
         MVC   PSECOND+19(29),=C'*COMMENT ELEMENT (J) DELETED*'                 
CM22F    MVI   MISSSW,C'Y'                                                      
         GOTO1 =A(PRNTKEY)                                                      
         MVI   MISSSW,C'N'                                                      
         B     CM24                                                             
*                                                                               
CM22F1   CLI   QOPT6,C'A'     SEE IF DEL ALL COMMENTS                           
         BE    *+12                                                             
         CLI   QOPT6,C'B'          SEE IF DOING BUY COMMENT                     
         BNE   CM22F                                                            
         BAS   RE,COMDELB                                                       
         MVC   PSECOND+19(29),=C'*COMMENT ELEMENT (B) DELETED*'                 
         B     CM22F                                                            
*                                                                               
CM24     DS    0H                                                               
         LA    R3,1(R3)                                                         
         CR    R3,R6                                                            
         BNL   CM16                NEXT ELEM                                    
         B     CM18                NEXT COM=                                    
*                                                                               
CM25     CLI   KEYSAVE+3,X'20'     SEE IF I JUST FINISHED BUYS                  
         BE    CM26                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'20'         NOW DO BUYS                                  
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         MVI   ELCODE,X'67'        IC= COMMENTS                                 
         B     CM12C                                                            
*                                                                               
CM25D    CLI   ELCODE,X'67'        SEE IF I JUST DID IC= COMMENTS               
         BE    CM25E               NO THEN DONE - DO NEXT RECORD                
         MVI   ELCODE,X'67'        MUST RESET ELCODE                            
         B     CM14                FOR NEXT RECORD                              
*                                                                               
CM25E    MVI   ELCODE,X'68'        NO - TRY FOR X'68' ELEMS                     
         B     CM15                                                             
*                                                                               
*                             BILL PROFS ON PRDS/ESTS                           
CM26     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,6                                                          
         GOTO1 HIGH                                                             
         B     CM28B                                                            
CM28     DS    0H                                                               
         CLI   WRTREC,C'Y'       SEE IF MUST WRITE BACK RECORD                  
         BNE   CM28A                                                            
         GOTO1 PUTPRT                                                           
         MVI   WRTREC,0                                                         
CM28A    GOTO1 SEQ                                                              
CM28B    DS    0H                                                               
         CLC   KEY(3),KEYSAVE                                                   
         BNE   CM31                                                             
         CLI   KEY+3,7                                                          
         BH    CM31                                                             
         LA    R0,PESTREC                                                       
         BE    *+8                                                              
         LA    R0,PPRDREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         LA    R6,PESTBILP                                                      
         CLI   KEY+3,7                                                          
         BE    *+8                                                              
         LA    R6,PPRDBILP                                                      
         USING BILPROF,R6                                                       
         MVC   DUB,SPACES                                                       
         LA    R3,BILCMNTS                                                      
         LA    R4,3                                                             
CM29     DS    0H                                                               
         MVC   DUB(6),1(R3)                                                     
         MVI   DUB+6,0                                                          
         GOTO1 BINSRCH,BSPARS,DUB                                               
*                                                                               
         CLI   BSPARS,1                                                         
         BE    CM29E                                                            
         L     RF,BSPARS                                                        
         MVI   6(RF),X'FF'         DO NOT PURGE STANDARD COMMENT REC            
         CLI   KEY+3,6             SEE IF DOING PRD RECORD                      
         BNE   CM29C                                                            
         CLI   QOPT6,C'P'          SEE IF DOING PRD COMMENT                     
         BE    CM29B                                                            
         CLI   QOPT6,C'A'     SEE IF DEL ALL COMMENTS                           
         BNE   CM29D                                                            
         MVI   6(RF),0             OK TO PURGE STANDARD COMMENT REC             
CM29B    BAS   RE,COMDELPE                                                      
         MVC   PSECOND+19(29),=C'*COMMENT ELEMENT (P) DELETED*'                 
         B     CM29D                                                            
*                                                                               
CM29C    CLI   KEY+3,7             SEE IF DOING EST RECORD                      
         BNE   CM31                                                             
         CLI   QOPT6,C'E'          SEE IF DOING EST COMMENT                     
         BE    CM29C20                                                          
         CLI   QOPT6,C'A'     SEE IF DEL ALL COMMENTS                           
         BNE   CM29D                                                            
         MVI   6(RF),0             OK TO PURGE STANDARD COMMENT REC             
CM29C20  BAS   RE,COMDELPE                                                      
         MVC   PSECOND+19(35),=C'*COMMENT ELEMENT (E) DELETED -CHAB*'           
*                                                                               
CM29D    GOTO1 =A(PRNTKEY)                                                      
         B     CM30                                                             
*                                                                               
CM29E    DS    0H                                                               
         CLI   QOPT2,C'G'          PURGEABLE ONLY?                              
         BE    CM29E2                                                           
         CLI   QOPT2,C'A'           SEE IF DOING ALL COMMENTS                   
         BNE   CM30                                                             
*                                                                               
CM29E2   DS    0H                                                               
         OC    DUB(6),DUB                                                       
         BZ    CM30                                                             
         CLI   QOPT6,C'A'     SEE IF DEL ALL COMMENTS                           
         BE    *+20                                                             
         CLI   QOPT6,C'P'          SEE IF DOING PRD COMMENT                     
         BE    *+12                                                             
         CLI   QOPT6,C'E'          SEE IF DOING EST COMMENT                     
         BNE   CM29F                                                            
         BAS   RE,COMDELPE                                                      
         CLI   KEY+3,7             EST ?                                        
         BE    *+14                                                             
         MVC   PSECOND+19(29),=C'*COMMENT ELEMENT (P) DELETED*'                 
         B     CM29F                                                            
         MVC   PSECOND+19(35),=C'*COMMENT ELEMENT (E) DELETED -CHAB*'           
*                                                                               
CM29F    MVI   MISSSW,C'Y'                                                      
         GOTO1 =A(PRNTKEY)                                                      
         MVI   MISSSW,C'N'                                                      
*                                                                               
CM30     LA    R3,7(R3)                                                         
         BCT   R4,CM29                                                          
*                                                                               
         CLI   KEY+3,7             ESTIMATE ??                                  
         BNE   CM28                                                             
         LA    R4,2                FOR PESTCOM2 (2ND STANDARD COMMENT)          
         LA    R5,PESTCOM                                                       
CM30B    XC    MYDUB,MYDUB                                                      
         MVC   MYDUB(6),0(R5)                                                   
         GOTO1 BINSRCH,BSPARS,MYDUB                                             
*                                                                               
         CLI   BSPARS,1                                                         
         BE    CM30E                                                            
         L     RF,BSPARS                                                        
         MVI   6(RF),X'FF'         DO NOT PURGE STANDARD COMMENT REC            
         CLI   QOPT6,C'E'                                                       
         BE    CM30C                                                            
         CLI   QOPT6,C'A'                                                       
         BNE   CM30D                                                            
         MVI   6(RF),0             OK TO PURGE STANDARD COMMENT REC             
CM30C    MVI   WRTREC,C'Y'                                                      
         XC    0(6,R5),0(R5)       DELETE COMMENT                               
         MVC   PSECOND+19(27),=C'*COMMENT FIELD (E) DELETED*'                   
*                                                                               
CM30D    DS    0H                                                               
         GOTO1 =A(PRNTKEY)                                                      
*                                                                               
         B     CM30T               TEST FOR 2ND COMMENT                         
*                                                                               
CM30E    DS    0H                                                               
         CLI   QOPT2,C'G'          PURGEABLE ONLY?                              
         BE    CM30E2                                                           
         CLI   QOPT2,C'A'                                                       
         BNE   CM30T               TEST FOR 2ND COMMENT                         
*                                                                               
CM30E2   DS    0H                                                               
         OC    MYDUB(6),MYDUB                                                   
         BZ    CM30T               TEST FOR 2ND COMMENT                         
         CLI   QOPT6,C'A'          FROM ALL PLACES                              
         BE    *+12                                                             
         CLI   QOPT6,C'E'                                                       
         BNE   CM30F                                                            
         MVI   WRTREC,C'Y'                                                      
         XC    0(6,R5),0(R5)       DELETE COMMENT                               
         MVC   PSECOND+19(27),=C'*COMMENT FIELD (E) DELETED*'                   
*                                                                               
CM30F    MVI   MISSSW,C'Y'                                                      
         GOTO1 =A(PRNTKEY)                                                      
         MVI   MISSSW,C'N'                                                      
*                                                                               
CM30T    DS    0H                  TEST FOR COMPLETION AND ...                  
         LA    R5,PESTCOM2         2ND COMMENT                                  
         BCT   R4,CM30B            2ND TIME THROUGH ?                           
         CLI   PESTCOM2,0          YES - ANYTHING IN THIS COMMENT ?             
         BE    CM28                NO - DONE WITH REC                           
***                   SEE IF NEED TO SHIFT PESTCOM2 TO PESTCOM                  
         OC    PESTCOM,PESTCOM     ANYTHING IN PESTCOM ?                        
         BNZ   CM28                YES - DONE WITH REC                          
         MVC   PESTCOM,PESTCOM2    NO - SHIFT PESTCOM2                          
         XC    PESTCOM2,PESTCOM2                                                
         B     CM28                DONE WITH REC                                
*                                                                               
         DROP  R6                                                               
*                                                                               
CM31     DS    0H              CHECK FOR USERP RECORDS                          
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'30'         USERP RECORDS                                
         GOTO1 HIGH                                                             
         B     CM31C                                                            
CM31A    DS    0H                                                               
         CLI   WRTREC,C'Y'       SEE IF MUST WRITE BACK RECORD                  
         BNE   CM31B                                                            
         GOTO1 PUTPRT                                                           
         MVI   WRTREC,0                                                         
CM31B    GOTO1 SEQ                                                              
CM31C    DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   CM32                                                             
         LA    R0,PBUYREC         READ USERP INTO PBUYREC                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'15'                                                     
         CLI   0(R2),X'15'                                                      
         BE    CM31D5                                                           
CM31D    BAS   RE,NEXTEL                                                        
         BNE   CM31A                                                            
         USING PUSRPELM,R2                                                      
*                                                                               
CM31D5   LA    R3,PUSRCOM1                                                      
         LA    R4,2                                                             
CM31E    MVC   DOUBLE(6),0(R3)                                                  
         MVI   DOUBLE+6,0                                                       
         GOTO1 BINSRCH,BSPARS,DOUBLE                                            
*                                                                               
         CLI   BSPARS,1                                                         
         BE    CM31J                                                            
         L     RF,BSPARS                                                        
         MVI   6(RF),X'FF'         DO NOT PURGE STANDARD COMMENT REC            
         CLI   QOPT6,C'U'          DOING USERP COMMENTS ?                       
         BE    CM31E20                                                          
         CLI   QOPT6,C'A'          DOING ALL COMMENTS ?                         
         BNE   CM31F                                                            
         MVI   6(RF),0             OK TO PURGE STANDARD COMMENT REC             
CM31E20  MVI   WRTREC,C'Y'                                                      
         XC    0(6,R3),0(R3)       DELETE COMMENT                               
         MVC   PSECOND+19(27),=C'*COMMENT FIELD (U) DELETED*'                   
*                                                                               
CM31F    DS    0H                                                               
         GOTO1 =A(PRNTKEY)                                                      
         B     CM31T               TEST FOR 2ND COMMENT                         
*                                                                               
CM31J    DS    0H                                                               
         CLI   QOPT2,C'G'          PURGEABLE ONLY?                              
         BE    CM31J2                                                           
         CLI   QOPT2,C'A'            SEE IF DOING ALL COMMENTS                  
         BNE   CM31T               TEST FOR 2ND COMMENT                         
*                                                                               
CM31J2   DS    0H                                                               
         OC    DOUBLE(6),DOUBLE                                                 
         BZ    CM31T               TEST FOR 2ND COMMENT                         
         CLI   QOPT6,C'A'          PURGING FROM ALL RECORDS                     
         BE    *+12                                                             
         CLI   QOPT6,C'U'          DOING USERP COMMENTS ?                       
         BNE   CM31M                                                            
         MVI   WRTREC,C'Y'                                                      
         XC    0(6,R3),0(R3)       DELETE COMMENT                               
         MVC   PSECOND+19(27),=C'*COMMENT FIELD (U) DELETED*'                   
*                                                                               
CM31M    MVI   MISSSW,C'Y'                                                      
         GOTO1 =A(PRNTKEY)                                                      
         MVI   MISSSW,C'N'                                                      
*                                                                               
CM31T    DS    0H                  TEST FOR COMPLETION AND ...                  
         LA    R3,6(R3)            2ND COMMENT                                  
         BCT   R4,CM31E            2ND TIME THROUGH ?                           
         OC    PUSRCOM2,PUSRCOM2   YES - ANYTHING IN THIS COMMENT ?             
         BZ    CM31D               NO - DONE WITH ELEMENT                       
***                   SEE IF NEED TO SHIFT PUSRCOM2 TO PUSRCOM1                 
         OC    PUSRCOM1,PUSRCOM1   ANYTHING IN PUSRCOM1 ?                       
         BNZ   CM31D               YES - DONE WITH ELEMENT                      
         MVC   PUSRCOM1,PUSRCOM2   NO - SHIFT PUSRCOM2                          
         XC    PUSRCOM2,PUSRCOM2                                                
         B     CM31D               DONE WITH ELEMENT                            
*                                                                               
         DROP  R2                                                               
*                              NOW CHECK FOR PUBFILE CLIENT ELEMS               
CM32     DS    0H                                                               
         MVI   WRTREC,0                                                         
         XC    PUBREC(33),PUBREC                                                
         XC    LASTPUB,LASTPUB                                                  
CM32B    CLI   WRTREC,C'Y'         SEE IF MUST WRITE BACK RECORD                
         BNE   CM32B20                                                          
         GOTO1 PUTPUB                                                           
         MVI   WRTREC,0                                                         
CM32B20  BAS   RE,NXTPUB                                                        
         CLI   PUBREC,X'FF'        END OF FILE                                  
         BE    CM34                                                             
         LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'14'                                                     
         CLI   0(R2),X'14'                                                      
         BE    CM32D                                                            
CM32C    BAS   RE,NEXTEL                                                        
         BNE   CM32B                                                            
CM32D    DS    0H                                                               
         USING PUBREPEL,R2                                                      
         CLI   PUBREPEL+1,45      CHECK FOR LARGE ELEMENT                       
         BL    CM32C              NO - SKIP                                     
         LA    R3,PUBCSCC1                                                      
         LA    R4,2           FOR BCT                                           
CM32E    DS    0H                                                               
         MVC   DOUBLE(6),1(R3)                                                  
         MVI   DOUBLE+6,0                                                       
         GOTO1 BINSRCH,BSPARS,DOUBLE                                            
*                                                                               
         CLI   BSPARS,1                                                         
         BE    CM32E50                                                          
         L     RF,BSPARS                                                        
         MVI   6(RF),X'FF'         DO NOT PURGE STANDARD COMMENT REC            
         CLI   QOPT6,C'L'          DOING PUB(L)ICATION COMMENTS ?               
         BE    CM32E20                                                          
         CLI   QOPT6,C'A'          DOING ALL COMMENTS ?                         
         BNE   CM32E30                                                          
         MVI   6(RF),0             OK TO PURGE STANDARD COMMENT REC             
CM32E20  MVI   WRTREC,C'Y'                                                      
         XC    0(7,R3),0(R3)       DELETE COMMENT & CONTROL BYTE                
         MVC   PSECOND+19(29),=C'*COMMENT FIELD (PUB) DELETED*'                 
*                                                                               
CM32E30  GOTO1 =A(PRNTKEY)                                                      
         B     CM32T               TEST FOR SECOND COMMENT                      
*                                                                               
CM32E50  DS    0H                                                               
         CLI   QOPT2,C'G'          PURGEABLE ONLY?                              
         BE    CM32E52                                                          
         CLI   QOPT2,C'A'       SEE IF DOING ALL COMMENTS                       
         BNE   CM32T               TEST FOR SECOND COMMENT                      
*                                                                               
CM32E52  DS    0H                                                               
         OC    DOUBLE(6),DOUBLE                                                 
         BZ    CM32T                                                            
         CLI   QOPT6,C'A'          PURGING FROM ALL RECORDS                     
         BE    *+12                                                             
         CLI   QOPT6,C'L'          DOING PUB(L)ICATION COMMENTS ?               
         BNE   CM32E70                                                          
         MVI   WRTREC,C'Y'                                                      
         XC    0(7,R3),0(R3)       DELETE COMMENT & CONTROL BYTE                
         MVC   PSECOND+19(29),=C'*COMMENT FIELD (PUB) DELETED*'                 
*                                                                               
CM32E70  MVI   MISSSW,C'Y'                                                      
         GOTO1 =A(PRNTKEY)                                                      
         MVI   MISSSW,C'N'                                                      
*                                                                               
CM32T    DS    0H                  TEST FOR COMPLETION AND ...                  
         LA    R3,7(R3)            2ND COMMENT                                  
         BCT   R4,CM32E            2ND TIME THROUGH ?                           
         OC    PUBCSC2,PUBCSC2     YES - ANYTHING IN COMMENT 2 ?                
         BZ    CM32C               NO - GO DO NEXT ELEM                         
*                    SEE IF NEED TO SHIFT COMMENT 2 TO COMMENT 1                
         OC    PUBCSC1,PUBCSC1     ANYTHING IN COMMENT 1 ?                      
         BNZ   CM32C               YES - GO DO NEXT ELEM (NO SHIFT)             
         MVC   PUBCSCC1(7),PUBCSCC2    SHIFT COMMENT AND CONTROL BYTE           
         XC    PUBCSCC2(7),PUBCSCC2    CLEAR                                    
         B     CM32C               GO DO NEXT ELEM                              
*                                                                               
CM34     DS    0H                                                               
         GOTO1 =A(RESULTS)                                                      
         B     EXIT                                                             
*                                                                               
         DROP  R2                                                               
*                                                                               
         DS    F                                                                
COMDEL   ST    RE,*-4                                                           
         MVI   WRTREC,C'Y'                                                      
         GOTO1 RECUP,DMCB,(1,PCONREC),0(R2),0                                   
*                                CLEAR END OF RECORD                            
         MVC   HALF,PCONREC+25                                                  
         LH    R1,HALF                                                          
         LA    RE,PCONREC                                                       
         LR    RF,RE                                                            
         AR    RE,R1                                                            
         LA    RF,1999(RF)                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         LA    R2,PCONREC+33     MUST RESET R2 TO BEGINNING OF RECORD           
*                                                                               
         L     RE,COMDEL-4         RETURN                                       
         BR    RE                                                               
*                                                                               
         DS    F                                                                
COMDELB  ST    RE,*-4                                                           
         MVI   WRTREC,C'Y'                                                      
         GOTO1 RECUP,DMCB,(1,PBUYREC),0(R2),0                                   
*                                CLEAR END OF RECORD                            
         MVC   HALF,PBUYREC+25                                                  
         LH    R1,HALF                                                          
         LA    RE,PBUYREC                                                       
         LR    RF,RE                                                            
         AR    RE,R1                                                            
         LA    RF,1999(RF)                                                      
         LA    RF,1000(RF)                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         LA    R2,PBUYREC+33     MUST RESET R2 TO BEGINNING OF RECORD           
*                                                                               
         L     RE,COMDELB-4        RETURN                                       
         BR    RE                                                               
*                                                                               
         DS    F                                                                
COMDELPE ST    RE,*-4                                                           
         MVI   WRTREC,C'Y'                                                      
*                                                                               
         CLI   R4,1                3RD COMMENT ?                                
         BNE   *+14                                                             
         XC    0(7,R3),0(R3)       DELETE LAST COMMENT                          
         B     COMOUT                                                           
*                                                                               
         CLI   R4,2                2ND COMMENT ?                                
         BNE   *+20                                                             
         MVC   0(7,R3),7(R3)       SHIFT OVER                                   
         XC    7(7,R3),7(R3)       DELETE LAST COMMENT                          
         B     COMOUT                                                           
*                                                                               
         MVC   0(14,R3),7(R3)      SHIFT OVER                                   
         XC    14(7,R3),14(R3)     DELETE LAST COMMENT                          
*                                                                               
COMOUT   L     RE,COMDELPE-4       RETURN                                       
         BR    RE                                                               
*                                                                               
         DS    F                                                                
COMDELJ  ST    RE,*-4                                                           
         MVI   WRTREC,C'Y'                                                      
         MVI   FOUND,C'N'                                                       
         ST    R6,TEMPR6           SAVE R6 AND                                  
         ST    R3,TEMPR3           R3                                           
         XC    TEMPELEM,TEMPELEM                                                
         ZIC   R6,1(R2)            L'ELEM                                       
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   TEMPELEM(0),0(R2)   SAVE ELEM                                    
*                                                                               
         LA    R3,TEMPELEM         PT TO ELEM                                   
         LA    R3,2(R3)                                                         
         LR    R1,R3                                                            
PTDATA   LA    R3,4(R3)            PT TO ACTUAL COMMENT                         
*                           SEE IF THIS IS THE COMMENT                          
         LR    R5,R3                                                            
         SR    R6,R6               COMPUTES LEN OF THIS COMMENT                 
BUMPCHAR LA    R5,1(R5)            BUMP NEXT CHAR                               
         LA    R6,1(R6)            INCREMENT CHAR COUNT                         
         CLI   0(R5),C','                                                       
         BE    LENCOMP                                                          
         CLI   0(R5),0             END OF ELEM ??                               
         BNE   BUMPCHAR                                                         
*                                                                               
LENCOMP  BCTR  R6,0                                                             
         CR    R6,R4                                                            
         BNE   NEXTCOMM                                                         
*                                                                               
         EX    R4,*+8              R4=LEN OF COMMENT                            
         B     *+10                                                             
         CLC   0(0,R3),MYDUB                                                    
         BNE   NEXTCOMM                                                         
*                                                                               
         MVI   FOUND,C'Y'                                                       
         LA    R5,TEMPELEM         PT TO ELEM                                   
         LA    R3,1(R4,R3)         END OF COMMENT                               
         LA    R4,6(R4)            L'OF ENTIRE COMMENT (COM=+ ',' + 1)          
         CLI   0(R3),C','                                                       
         BNE   ELEMLEN                                                          
*                                                                               
*                                  SHIFT REST OF ELEM                           
         ST    R1,TEMPR1           SAVE LOCATION OF INSERTION                   
         SR    R1,R5               DISPLACEMENT                                 
         ZIC   R6,1(R5)            L'ELEM                                       
         SR    R6,R1                                                            
         SR    R6,R4               L'REST OF THE ELEM                           
*                                                                               
         L     R1,TEMPR1           RESTORE LOC OF INSERTION                     
         CLI   0(R1),C','          SEE IF 1ST COM= IS DELETED                   
         BE    *+18                                                             
         EX    R6,*+8                                                           
         B     ELEMLEN                                                          
         MVC   0(0,R1),1(R3)                                                    
*                                                                               
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R3)                                                    
*                                                                               
ELEMLEN  ZIC   R6,1(R5)           L'OLD ELEM                                    
         SR    R6,R4                                                            
         STCM  R6,1,1(R5)         L'NEW ELEM                                    
         B     DELELEM                                                          
*                                                                               
NEXTCOMM LA    R3,1(R3)                                                         
         CLI   0(R3),0             END OF RECORD ?                              
         BER   RE                  YES                                          
         CLI   0(R3),C','                                                       
         BNE   NEXTCOMM                                                         
         LR    R1,R3                                                            
         LA    R3,1(R3)            POINT TO COM=                                
         B     PTDATA                                                           
*                                                                               
DELELEM  GOTO1 RECUP,DMCB,(1,PJOBREC),0(R2),0                                   
*                                                                               
         CLI   1(R5),2             ANY DATA IN ELEM                             
         BNH   CLEAR                                                            
*                                                                               
         GOTO1 RECUP,DMCB,(1,PJOBREC),TEMPELEM,0(R2),0                          
*                                CLEAR END OF RECORD                            
CLEAR    MVC   HALF,PJOBREC+25                                                  
         LH    R1,HALF                                                          
         LA    RE,PJOBREC                                                       
         LR    RF,RE                                                            
         AR    RE,R1                                                            
         LA    RF,1999(RF)                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         LA    R2,PJOBREC+33     MUST RESET R2 TO BEGINNING OF RECORD           
*                                                                               
         L     RE,COMDELJ-4        RETURN                                       
         L     R6,TEMPR6           RESTORE R6 AND                               
         L     R3,TEMPR3           R3                                           
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*                                  HEADERS - CLIENTS/PRODUCTS                   
HDRS     DS    0H                                                               
         MVI   RCSUBPRG,50                                                      
*                                  SET BSPARS                                   
         XC    BSPARS,BSPARS                                                    
         L     RF,ABSTAB                                                        
         ST    RF,BSPARS+4                                                      
         LA    RF,8                LENGTH                                       
         ST    RF,BSPARS+12                                                     
         LA    RF,6                KEY                                          
         ST    RF,BSPARS+16                                                     
         L     RF,=A(BSTAB7)                                    L01             
         ST    RF,BSPARS+20                                                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'02'                                                      
         CLC   QCLIENT(3),=C'ALL'                                               
         BE    HD1                                                              
         CLC   QCLIENT,=C'   '                                                  
         BE    HD1                                                              
         MVC   KEY+4(3),QCLIENT                                                 
HD1      DS    0H                                                               
         GOTO1 HIGH                                                             
         B     HD2B                                                             
HD2      DS    0H                                                               
         GOTO1 SEQ                                                              
HD2B     DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   HD2L                                                             
         CLC   QCLIENT(3),=C'ALL'                                               
         BE    HD2B5                                                            
         CLC   QCLIENT,=C'   '                                                  
         BE    HD2B5                                                            
         CLC   KEY+4(3),QCLIENT      MUST BE ONE CLIENT                         
         BNE   HD2L                                                             
*                                                                               
HD2B5    DS    0H                                                               
         LA    RF,PCLTREC                                                       
         ST    RF,AREC                                                          
         CLI   QOPT2,C'A'                                                       
         BE    HD2F                                                             
         CLI   QOPT2,C'G'          PURGEABLE ONLY?                              
         BE    HD2F                                                             
         CLC   QCLIENT(3),=C'ALL'                                               
         BE    HD2B8                                                            
         CLC   QCLIENT,=C'   '                                                  
         BE    HD2B8                                                            
         B     HD2F                   MUST BE ONE CLIENT                        
*                                                                               
HD2B8    DS    0H                                                               
         GOTO1 GETPRT                                                           
         CLC   PCLTNAME(6),=C'*PURGE'                                           
         BE    HD2F                                                             
         CLC   PCLTNAME(7),=C'**PURGE'                                          
         BE    HD2F                                                             
         CLC   PCLTNAME(8),=C'***PURGE'                                         
         BE    HD2F                                                             
*                                                                               
         CLI   QOPT2,C'B'        SEE IF ALSO CHECKING FOR "PURGE"               
         BNE   HD2                                                              
         CLC   PCLTNAME(5),=C'PURGE '                                           
         BE    HD2F                                                             
         B     HD2                                                              
*                                                                               
*                                                                               
HD2F     DS    0H                                                               
         XC    DOUBLE,DOUBLE                                                    
         MVC   DOUBLE(6),KEY+4                                                  
         GOTO1 BINSRCH,BSPARS,(1,DOUBLE)                                        
*                                                                               
         OC    BSPARS+1(3),BSPARS+1                                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         B     HD2                                                              
*                                                                               
HD2L     DS    0H                                                               
         OC    BSPARS+8(4),BSPARS+8                                             
         BZ    EXIT                                                             
*                                                                               
*     IF QOPT1=S, DELETE ALL CLEARANCE STATUS RECORDS FOR THE ONE               
*     CLIENT IN QCLIENT - NO OTHER PURGING IS DONE FOR QOPT1=S                  
*                                                                               
         CLI   QOPT1,C'S'                                                       
         BNE   HD4                                                              
*                                                                               
HD3      DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'25'         CLEARANCE STATUS RECORD                      
         MVC   KEY+4(3),QCLIENT                                                 
         GOTO1 HIGH                                                             
         B     HD3B                                                             
HD3A     DS    0H                                                               
         GOTO1 SEQ                                                              
HD3B     DS    0H                                                               
         CLC   KEY(7),KEYSAVE                                                   
         BE    HD3D                GO FLAG FOR DELETION                         
         GOTO1 =A(RESULTS)                                                      
         B     EXIT                DONE                                         
HD3D     DS    0H                                                               
         OI    KEY+25,X'80'        FLAG FOR DELETION                            
         GOTO1 WRT                                                              
         MVC   PSECOND(11),=C'**DELETED**'                                      
         GOTO1 =A(PRNTKEY)                                                      
         AP    PRGCNT,=P'1'   PRGCNT NOT USED IN "RESULTS" FOR THIS RTN         
         B     HD3A                NEXT RECORD                                  
*                                                                               
HD4      LA    R5,CTYPTAB                                                       
*                                                                               
HD5      CLI   0(R5),X'FF'        DONE                                          
         BE    HD11                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVC   KEY+3(1),0(R5)      LOOK FOR TYPE                                
         GOTO1 HIGH                                                             
         B     HD6B                                                             
HD6      DS    0H                                                               
         GOTO1 SEQ                                                              
HD6B     DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   HD10                 NOT FOUND GO DO NEXT TYPE                   
         CLI   KEY+3,X'20'          SEE IF BUYREC                               
         BE    HD7R                 YES - PROCESS ALL BUYS                      
*                             FOR OTHER RECORD TYPES SKIP DELETES               
         TM    KEY+25,X'80'         SEE IF DELETED                              
         BO    HD6                  YES - SKIP THIS RECORD                      
*                                                                               
HD7      CLI   KEY+3,X'27'                                                      
         BNE   HD7A                                                             
         CLI   KEY+10,X'FF'        ALL CLIENTS ?                                
         BE    HD6                 YES - DO NOT DELETE                          
         MVC   DOUBLE(3),KEY+10    CLIENT                                       
         B     HD7S                                                             
*                                                                               
HD7A     CLI   KEY+3,X'28'                                                      
         BNE   HD7B                                                             
         CLI   KEY+10,X'FF'        ALL CLIENTS?                                 
         BE    HD6                 YES - DO NOT DELETE                          
         MVC   DOUBLE(3),KEY+10    CLIENT                                       
         B     HD7S                                                             
*                                                                               
HD7B     CLI   KEY+3,X'2A'         YRLY PUB CIRC RECORD                         
         BNE   HD7D                                                             
         CLI   KEY+17,X'FF'        ALL CLIENTS?                                 
         BE    HD6                 IF SO, DON'T DELETE                          
         MVC   DOUBLE(3),KEY+17    CLIENT                                       
         B     HD7S                                                             
*                                                                               
HD7D     CLI   KEY+3,X'2B'         SPACE RECORD                                 
         BNE   HD7E                                                             
         CLI   KEY+10,X'FF'        ALL RECORDS?                                 
         BE    HD6                 IF SO, DON'T DELETE                          
         MVC   DOUBLE(3),KEY+10                                                 
         B     HD7S                                                             
*                                                                               
HD7E     CLI   KEY+3,X'3A'         NEW CLNT GRP DIR ONLY REC                    
         BNE   HD7G                                                             
         MVC   DOUBLE(3),KEY+7     CLIENT                                       
         B     HD7S                                                             
*                                                                               
HD7G     CLI   KEY+3,X'3D'         NEW CLNT GRP DIR ONLY REC                    
         BNE   HD7R                                                             
         MVC   DOUBLE(3),KEY+10    CLIENT                                       
         B     HD7S                                                             
*                                                                               
HD7R     MVC   DOUBLE(3),KEY+4     CLIENT                                       
HD7S     MVI   DOUBLE+6,0                                                       
         GOTO1 BINSRCH,BSPARS,DOUBLE                                            
*                                                                               
         CLI   BSPARS,1                                                         
         BE    HD6                                                              
         L     RF,BSPARS                                                        
         CLI   5(R5),X'00'                                                      
         BE    *+8                                                              
         MVI   6(RF),X'FF'         MEANS CLIENT CANNOT BE PURGED                
*                                                                               
         CLI   5(R5),X'00'                                                      
         BNE   HD7X                                                             
         CLI   6(RF),X'FF'         SEE IF CLIENT CANNOT BE PURGED               
         BE    HD7X                                                             
*                                                                               
         CLI   QOPT5,C'Y'          SEE IF PURGING CLIENT'S RECORDS              
         BNE   HD7X                                                             
         OI    KEY+25,X'80'                                                     
         GOTO1 WRT                                                              
         AP    1(4,R5),=P'1'                                                    
         MVC   PSECOND(11),=C'**DELETED**'                                      
*                                                                               
HD7X     GOTO1 =A(PRNTKEY)                                                      
         B     HD6                                                              
*                                                                               
HD10     LA    R5,6(R5)                                                         
         B     HD5                                                              
*                                                                               
*                  ******* CHECK CLIENT RECORDS FOR DRD CLIENT ELEMENT          
HD11     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'02'         CLIENT HDR                                   
         GOTO1 HIGH                                                             
         B     HD12B                                                            
HD12     DS    0H                                                               
         GOTO1 SEQ                                                              
HD12B    DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   HD16                 DONE - GO CHECK PUB                         
*                                                                               
         LA    RF,PCLTREC                                                       
         ST    RF,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         LA    R2,PCLTREC+33                                                    
         MVI   ELCODE,X'30'        DRD CLIENT ELEMENT                           
         BAS   RE,NEXTEL                                                        
         BNE   HD12                ELEMENT NOT FOUND - GET NEXT REC             
*                                  ELEMENT FOUND                                
         XC    DOUBLE,DOUBLE                                                    
         MVC   DOUBLE(3),2(R2)     DRD CLIENT CODE                              
         MVI   DOUBLE+6,0                                                       
         GOTO1 BINSRCH,BSPARS,DOUBLE                                            
*                                                                               
         CLI   BSPARS,1                                                         
         BE    HD12            CLIENT CODE NOT FOUND - GET NEXT REC             
*                              CODE FOUND                                       
         MVC   P+9(12),=C'DRD OVERRIDE'                                         
         CLI   QOPT5,C'Y'        SEE IF DELETING CLIENT'S RECORDS               
         BNE   HD13              NO                                             
         L     RF,BSPARS                                                        
         CLI   6(RF),X'FF'       SEE IF CLIENT CANNOT BE PURGED                 
         BE    HD13              CANNOT                                         
         GOTO1 RECUP,DMCB,(1,PCLTREC),0(R2),0                                   
*                                CLEAR END OF RECORD                            
         MVC   HALF,PCLTREC+25                                                  
         LH    R1,HALF                                                          
         LA    RE,PCLTREC                                                       
         AR    RE,R1                                                            
         SR    RF,RF                                                            
         LA    RF,PCLTREC                                                       
         LA    RF,469(RF)                                                       
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         MVC   PSECOND(19),=C'**ELEMENT DELETED**'                              
HD13     GOTO1 =A(PRNTKEY)                                                      
         CLI   QOPT5,C'Y'      WRITE BACK RECORD ?                              
         BNE   HD12            NO - NEXT RECORD                                 
         L     RF,BSPARS                                                        
         CLI   6(RF),X'FF'       SEE IF CLIENT CANNOT BE PURGED                 
         BE    HD12              CANNOT - NEXT RECORD                           
         GOTO1 PUTPRT                                                           
         B     HD12            NEXT RECORD                                      
*                                                                               
HD16     DS    0H                                                               
         XC    PUBREC(33),PUBREC                                                
         XC    LASTPUB,LASTPUB                                                  
         DS    0H                                                               
HD17     BAS   RE,NXTPUB                                                        
         CLI   PUBKEY,X'FF'        END OF FILE                                  
         BE    HD20                                                             
         GOTO1 =A(CKPUB)                                                        
         B     HD17                                                             
*                                                                               
HD20     GOTO1 =A(RESULTS)                                                      
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                  PUB LIST RECORDS                             
LSTS     DS    0H                                                               
         MVI   RCSUBPRG,60                                                      
*                                  SET BSPARS                                   
         XC    BSPARS,BSPARS                                                    
         L     RF,ABSTAB                                                        
         ST    RF,BSPARS+4                                                      
         LA    RF,7                L'RECORD                                     
         ST    RF,BSPARS+12                                                     
         LA    RF,6                L'KEY (CLIENT + LIST CODE)                   
         ST    RF,BSPARS+16                                                     
         L     RF,=A(BSTAB7)                                                    
         ST    RF,BSPARS+20                                                     
*                                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'17'                                                      
         CLI   QPUB+1,C'L'         ONE PUB LIST ONLY ?                          
         BNE   LSTSA               NO                                           
*                                                                               
         CLC   QCLIENT,=C'ALL'                                                  
         BNE   *+10                                                             
         MVC   QCLIENT,=C'ZZZ'     SWITCH ALL TO ZZZ                            
*                                  IF ONE LIST REQUEST                          
         MVC   KEY+4(3),QCLIENT    YES - SET CLT                                
         MVC   KEY+7(3),QPUB+2           AND LIST CODE                          
         MVC   DOUBLE(6),KEY+4     SAVE FOR TEST IN LSTS2B                      
         B     LSTS1                                                            
LSTSA    CLC   QCLIENT,=C'ALL'                                                  
         BE    LSTS1                                                            
         CLC   QCLIENT,=C'   '                                                  
         BE    LSTS1                                                            
         MVC   KEY+4(3),QCLIENT                                                 
LSTS1    GOTO1 HIGH                                                             
         B     LSTS2B                                                           
LSTS2    DS    0H                                                               
         GOTO1 SEQ                                                              
LSTS2B   DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   LSTS4                                                            
         CLI   QPUB+1,C'L'         ONE PUB LIST ONLY ?                          
         BNE   LSTS2C              NO                                           
         CLC   KEY+4(6),DOUBLE     YES                                          
         BNE   LSTS4                                                            
         B     LSTS3                                                            
LSTS2C   CLC   QCLIENT,=C'ALL'                                                  
         BE    LSTS2D                                                           
         CLC   QCLIENT,=C'   '                                                  
         BE    LSTS2D                                                           
         CLC   KEY(7),KEYSAVE                                                   
         BNE   LSTS4                                                            
*                                                                               
LSTS2D   CLI   QOPT2,C'A'                                                       
         BE    LSTS3                                                            
         CLI   QOPT2,C'G'          PURGEABLE ONLY?                              
         BE    LSTS3                                                            
*                                                                               
         LA    RF,PCONREC          READ INTO CONREC                             
         ST    RF,AREC                                                          
         GOTO1 GETPRT                                                           
         LA    R2,PCONREC+33                                                    
         CLI   0(R2),X'10'         PLISDTEL ?                                   
         BNE   LSTS2               NO - NEXT RECORD                             
         USING PLISDTEL,R2                                                      
         CLC   PLISDESC(6),=C'*PURGE'                                           
         BE    LSTS3                                                            
         CLC   PLISDESC(7),=C'**PURGE'                                          
         BE    LSTS3                                                            
         CLC   PLISDESC(8),=C'***PURGE'                                         
         BE    LSTS3                                                            
*                                                                               
         CLI   QOPT2,C'B'        SEE IF ALSO CHECKING FOR "PURGE"               
         BNE   LSTS2                                                            
         CLC   PLISDESC(5),=C'PURGE '                                           
         BE    LSTS3                                                            
         B     LSTS2                                                            
*                                                                               
         DROP  R2                                                               
*                                                                               
LSTS3    DS    0H                                                               
         XC    DOUBLE,DOUBLE                                                    
         MVC   DOUBLE(6),KEY+4                                                  
         GOTO1 BINSRCH,BSPARS,(1,DOUBLE)                                        
*                                                                               
         OC    BSPARS+1(3),BSPARS+1                                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         B     LSTS2                                                            
*                                                                               
LSTS4    DS    0H                                                               
         OC    BSPARS+8(4),BSPARS+8     ANYTHING IN TABLE ?                     
         BZ    EXIT                     NO                                      
*                                  CHECK FOR PUB LIST IN BUYS                   
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'20'                                                      
         GOTO1 HIGH                                                             
         B     LSTS6B                                                           
LSTS6    DS    0H                                                               
*NOP*    CLI   WRTREC,C'Y'         SEE IF MUST WRITE BACK RECORD                
*NOP*    BNE   LSTS6A                                                           
*NOP*    GOTO1 PUTPRT                                                           
LSTS6A   GOTO1 SEQ                                                              
LSTS6B   DS    0H                                                               
*NOP*    MVI   WRTREC,0                                                         
         CLC   KEY(4),KEYSAVE                                                   
         BNE   LSTS7                                                            
         LA    RF,PBUYREC                                                       
         ST    RF,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         MVC   DOUBLE(3),PBUYKCLT                                               
         MVC   DOUBLE+3(3),PBDLIST                                              
         MVI   DOUBLE+6,0                                                       
         GOTO1 BINSRCH,BSPARS,DOUBLE                                            
*                                                                               
         CLI   BSPARS,1            PUB LIST IN BUY REC NOT IN TABLE             
         BE    LSTS6               NEXT BUY REC                                 
         L     RF,BSPARS                                                        
         MVI   6(RF),X'FF'                                                      
*                                                                               
         MVC   WORK+50(3),PBDLIST     SET PUB LIST CODE FOR PRINT               
*NOP*    CLI   QOPT5,C'Y'          CLEAR LIST FROM RECORDS ?                    
*NOP*    BNE   LSTS6P              NO                                           
*NOP*    XC    PBDLIST,PBDLIST                                                  
*NOP*    MVC   PSECOND+19(23),=C'*PUB LIST CODE CLEARED*'                       
*NOP*    MVI   WRTREC,C'Y'                                                      
LSTS6P   DS    0H                                                               
         GOTO1 =A(PRNTKEY)                                                      
         B     LSTS6                                                            
*                                                                               
LSTS7    DS    0H                  CHECK FOR PUB LIST IN JOB RECORD             
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'15'                                                      
         GOTO1 HIGH                                                             
         B     LSTS7D                                                           
LSTS7B   DS    0H                                                               
         CLI   WRTREC,C'Y'         SEE IF MUST WRITE BACK RECORD                
         BNE   LSTS7C                                                           
         GOTO1 PUTPRT                                                           
LSTS7C   GOTO1 SEQ                                                              
LSTS7D   DS    0H                                                               
         MVI   WRTREC,0                                                         
         CLC   KEY(4),KEYSAVE                                                   
         BNE   LSTS10E             DONE                                         
         OC    KEY+16(6),KEY+16    JOB HEADER RECORD ?                          
         BNZ   LSTS7C              NO - SKIP                                    
         LA    RF,PJOBREC                                                       
         ST    RF,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         LA    R2,PJOBREC+33                                                    
         CLI   0(R2),X'15'                                                      
         BE    LSTS7F                                                           
         MVI   ELCODE,X'15'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   LSTS7C              NEXT RECORD                                  
*                                                                               
LSTS7F   DS    0H                                                               
         USING PJOBELEM,R2                                                      
         MVC   DOUBLE(3),PJOBKCLT                                               
         MVC   DOUBLE+3(3),PJOBPLIS                                             
         MVI   DOUBLE+6,0                                                       
         GOTO1 BINSRCH,BSPARS,DOUBLE                                            
*                                                                               
         CLI   BSPARS,1            PUB LIST IN JOB REC NOT IN TABLE             
         BE    LSTS7B              NEXT JOB RECORD                              
         L     RF,BSPARS                                                        
         MVI   6(RF),X'FF'                                                      
*                                                                               
         MVC   WORK+50(3),PJOBPLIS    SET PUB LIST CODE FOR PRINT               
         CLI   QOPT5,C'Y'          CLEAR LIST FROM RECORDS ?                    
         BNE   LSTS7P              NO                                           
         XC    PJOBPLIS,PJOBPLIS                                                
         MVC   PSECOND+19(23),=C'*PUB LIST CODE CLEARED*'                       
         MVI   WRTREC,C'Y'                                                      
LSTS7P   DS    0H                                                               
         GOTO1 =A(PRNTKEY)                                                      
         B     LSTS7B                                                           
*                                                                               
LSTS10E  GOTO1 =A(RESULTS)                                                      
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*                             GET FIRST/NEXT PUB                                
NXTPUB   NTR1                                                                   
*                                                                               
         XC    SVPUBKEY,SVPUBKEY                                                
         XC    SVLTLKEY,SVLTLKEY                                                
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         MVC   KEY+1(6),LASTPUB                                                 
         IC    R1,KEY+6                                                         
         LA    R1,1(R1)                                                         
         STC   R1,KEY+6                                                         
         MVC   KEY+7(2),PAGYKAGY                                                
         GOTO1 HIGHPUB                                                          
         B     NP2B                                                             
NP2      DS    0H                                                               
         GOTO1 SEQPUB                                                           
NP2B     DS    0H                                                               
         MVI   PUBKMED,X'FF'       SET EOF                                      
         CLC   KEY(1),KEYSAVE                                                   
         BNE   NPX                                                              
         CLC   KEY+7(2),PAGYKAGY        MUST BE RIGHT AGENCY                    
         BNE   NP2                                                              
         CLI   KEY+9,X'81'                                                      
         BNE   NP2                                                              
         MVC   LASTPUB,KEY+1                                                    
         GOTO1 GETNAME                                                          
         MVC   SVPUBKEY,KEY                                                     
         L     RF,ALTLREC                                                       
         XC    0(50,RF),0(RF)                                                   
         MVI   KEY+9,X'85'         LOOK FOR "LITTLE" REC                        
         GOTO1 HIGHPUB                                                          
NP2D     CLC   KEY(10),KEYSAVE     LITTLE REC FOUND?                            
         BE    NP5                 YES                                          
*                                  NO                                           
NP2K     MVC   KEY(9),PUBKEY         MUST RESTORE KEY FOR PRINTING              
         B     NPX                                                              
*                                                                               
NP5      GOTO1 GETLTL                                                           
         MVC   SVLTLKEY,KEY                                                     
*                                                                               
NPX      DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NEXTEL+2                                                         
         LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        TABLE OF RECORDS TYPES USED FOR HDR PURGES                             
*        TABLE FORMAT                                                           
*        FIRST BYTE = RECORD CODE (KEY+3)                                       
*        NEXT 4 BYTES = COUNT OF RECORDS PURGED                                 
*        LAST BYTE X'00' = RECORD CAN BE PURGED                                 
*                  X'01' = RECORD CANNOT BE PURGED                              
*                                                                               
CTYPTAB  DC    X'06',PL4'0',X'01'  PRODUCTS                                     
         DC    X'07',PL4'0',X'01'  ESTIMATES                                    
         DC    X'08',PL4'0',X'01'  BILLS                                        
         DC    X'09',PL4'0',X'01'  EST BUCKETS                                  
         DC    X'15',PL4'0',X'01'  ADCODES                                      
         DC    X'20',PL4'0',X'01'  BUYS                                         
*                                                                               
         DC    X'10',PL4'0',X'00'  CONTRACTS                                    
         DC    X'25',PL4'0',X'00'  CLEARANCE STATUS RECORDS                     
         DC    X'26',PL4'0',X'00'  PG SPECIAL COST RECORDS                      
         DC    X'27',PL4'0',X'00'  FSI RECORDS                                  
         DC    X'28',PL4'0',X'00'  SCU RECORDS                                  
         DC    X'29',PL4'0',X'00'  ISSUE DATE RECORDS                           
         DC    X'2A',PL4'0',X'00'  YRLY PUB CIRC RECORDS                        
         DC    X'2B',PL4'0',X'00'  SPACE RECORDS                                
*                                                                               
         DC    X'3A',PL4'0',X'00'  CLIENT GROUP DIRECTORY ONLY RECORDS          
         DC    X'3D',PL4'0',X'00'  CLIENT GROUP DIRECTORY ONLY RECORDS          
*                                                                               
         DC    X'03',PL4'0',X'00'  DIVISIONS                                    
         DC    X'04',PL4'0',X'00'  REGIONS                                      
         DC    X'05',PL4'0',X'00'  DISTRICTS                                    
         DC    X'0A',PL4'0',X'00'  PG EST RECS                                  
         DC    X'0B',PL4'0',X'00'  GF EST RECS                                  
         DC    X'14',PL4'0',X'00'  AOR RECORDS                                  
         DC    X'17',PL4'0',X'00'  PUB LISTS                                    
         DC    X'18',PL4'0',X'00'  BUDGETS                                      
         DC    X'41',PL4'0',X'00'  NVTEXTS                                      
         DC    X'4C',PL4'0',X'00'  BILL FORMULA RECORDS                         
         DC    X'50',PL4'0',X'00'  INV MATCHING RECORDS                         
         DC    X'90',PL4'0',X'00'  EST/PUB UPLOAD RECORD                        
         DC    X'FF'             END OF TABLE                                   
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
*        PUB RECORD PURGE                                                       
*                                                                               
***********************************************************************         
*                                                                               
PUBS     CSECT                                                                  
         NMOD1 0,PUBS                                                           
*                             ************************************              
*                             NOTE: DO NOT USE R7 IN THIS ROUTINE.              
*                             IT IS NEEDED FOR ADDRESSING PRIOR                 
*                             ROUTINES - NXTPUB AND NEXTEL.                     
*                             ************************************              
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
*                                                                               
         MVI   RCSUBPRG,10                                                      
         XC    BSPARS,BSPARS                                                    
         L     RF,ABSTAB                                                        
         ST    RF,BSPARS+4                                                      
         LA    RF,8           L'RECORD                                          
         ST    RF,BSPARS+12                                                     
         LA    RF,6           L'KEY                                             
         ST    RF,BSPARS+16                                                     
         L     RF,=A(BSTAB8)                                      L01           
         ST    RF,BSPARS+20                                                     
*                                                                               
         CLI   QPUB,C' '           ONE PUB DELETION OPTION?                     
         BE    PB1                 NO                                           
         XC    LASTPUB,LASTPUB                                                  
         GOTO1 PUBVAL,DMCB,QPUB,LASTPUB                                         
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         MVC   KEY+1(6),LASTPUB                                                 
         MVC   KEY+7(2),PAGYKAGY                                                
         MVI   KEY+9,X'81'         LOOK FOR RECORD                              
         GOTO1 HIGHPUB                                                          
         CLC   KEY(10),KEYSAVE                                                  
         BE    PB0                                                              
         MVC   P(16),=C'RECORD NOT FOUND'                                       
         GOTO1 REPORT                                                           
         B     PBEXIT                                                           
*                                                                               
PB0      DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         MVC   KEY+1(6),LASTPUB                                                 
         MVC   KEY+7(2),PAGYKAGY                                                
         MVI   KEY+9,X'85'         LOOK FOR "LITTLE" REC                        
         MVC   PUBKEY(9),KEY       FOR USE IN PB3                               
         L     RF,ALTLREC                                                       
         MVI   0(RF),0                                                          
         GOTO1 HIGHPUB                                                          
         CLC   KEY(10),KEYSAVE     LITTLE REC FOUND?                            
         BNE   PB3                 NO                                           
         L     RF,ALTLREC          YES                                          
         MVI   0(RF),1                                                          
         B     PB3                                                              
*                                                                               
PB1      XC    PUBREC(33),PUBREC                                                
         XC    LASTPUB,LASTPUB                                                  
PB2      DS    0H                                                               
         BAS   RE,NXTPUB                                                        
         CLI   PUBKEY,X'FF'        END OF FILE                                  
         BE    PB4                                                              
         CLI   QOPT2,C'A'                                                       
         BE    PB3                                                              
         CLI   QOPT2,C'G'          PURGEABLE ONLY?                              
         BE    PB3                                                              
         CLC   PUBNAME(6),=C'*PURGE'                                            
         BE    PB3                                                              
         CLC   PUBZNAME(6),=C'*PURGE'                                           
         BE    PB3                                                              
         CLC   PUBNAME(7),=C'**PURGE'                                           
         BE    PB3                                                              
         CLC   PUBZNAME(7),=C'**PURGE'                                          
         BE    PB3                                                              
         CLC   PUBNAME(8),=C'***PURGE'                                          
         BE    PB3                                                              
         CLC   PUBZNAME(8),=C'***PURGE'                                         
         BE    PB3                                                              
*                                                                               
         CLI   QOPT2,C'B'        SEE IF ALSO CHECKING FOR "PURGE"               
         BNE   PB2                                                              
         CLC   PUBNAME(5),=C'PURGE '                                            
         BE    PB3                                                              
         CLC   PUBZNAME(5),=C'PURGE '                                           
         BE    PB3                                                              
         B     PB2                                                              
*                                                                               
PB3      DS    0H                                                               
         XC    DOUBLE,DOUBLE                                                    
         MVC   DOUBLE(6),PUBKPUB                                                
         L     RF,ALTLREC                                                       
         CLI   0(RF),0                                                          
         BE    *+8                                                              
         MVI   DOUBLE+6,C'L'                                                    
         GOTO1 BINSRCH,BSPARS,(1,DOUBLE)                                        
*                                                                               
         OC    BSPARS+1(3),BSPARS+1                                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   QPUB,C' '           ONE PUB DELETION OPTION?                     
         BE    PB2                 NO                                           
PB4      DS    0H                                                               
         OC    BSPARS+8(4),BSPARS+8                                             
         BZ    PBEXIT                                                           
         XC    DOUBLE,DOUBLE                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'21'                                                      
PB4B     DS    0H                                                               
         GOTO1 HIGH                                                             
         B     PB6A                                                             
PB6      DS    0H                                                               
         GOTO1 SEQ                                                              
PB6A     DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   PB8                                                              
         CLC   KEY+7(6),DOUBLE                                                  
         BE    PB7                                                              
         MVC   DOUBLE(6),KEY+7                                                  
         MVI   DOUBLE+7,0                                                       
         GOTO1 BINSRCH,BSPARS,DOUBLE                                            
*                                                                               
         CLI   BSPARS,1                                                         
         BE    PB6                 NOT IN TABLE                                 
         L     RF,BSPARS                                                        
         MVI   7(RF),X'FF'         SET FOUND ON BUY FILE                        
         MVI   DOUBLE+7,X'FF'                                                   
PB6B     DS    0H                                                               
         GOTO1 =A(PRNTKEY)                                                      
         B     PB6                                                              
*                                                                               
PB7      DS    0H                                                               
         CLI   DOUBLE+7,0                                                       
         BE    PB6                                                              
         B     PB6B                                                             
*                                                                               
PB8      DS    0H                                                               
*                                  NOW DO CONTRACTS                             
*                                                                               
PB80     DS    0H                                                               
         XC    KEY,KEY                                                          
         XC    DOUBLE,DOUBLE                                                    
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'10'         CONTRACTS                                    
         GOTO1 HIGH                                                             
         B     PB802                                                            
*                                                                               
PB801    DS    0H                                                               
         GOTO1 SEQ                                                              
PB802    DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   PB8A                                                             
*                                                                               
         TM    KEY+25,X'80'       SEE IF DELETED                                
         BO    PB801              SKIP                                          
*                                                                               
         BAS   RE,PBSRCH                                                        
         B     PB801                                                            
*                                                                               
PB8A     DS    0H                                                               
         XC    KEY,KEY                                                          
         XC    DOUBLE,DOUBLE                                                    
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'27'         FSI RECORD                                   
         GOTO1 HIGH                                                             
         B     PB8A2                                                            
*                                                                               
PB8A1    DS    0H                                                               
         GOTO1 SEQ                                                              
PB8A2    DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   PB8B                                                             
         BAS   RE,PBSRCH                                                        
         B     PB8A1                                                            
*                                                                               
PB8B     DS    0H                                                               
         XC    KEY,KEY                                                          
         XC    DOUBLE,DOUBLE                                                    
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'28'         SPACE CU RECORD                              
         GOTO1 HIGH                                                             
         B     PB8B2                                                            
*                                                                               
PB8B1    DS    0H                                                               
         GOTO1 SEQ                                                              
PB8B2    DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   PB8C                                                             
         BAS   RE,PBSRCH                                                        
         B     PB8B1                                                            
*                                                                               
PB8C     DS    0H                                                               
         XC    KEY,KEY                                                          
         XC    DOUBLE,DOUBLE                                                    
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'29'         ISSUE DATE RECORD                            
         GOTO1 HIGH                                                             
         B     PB8C2                                                            
*                                                                               
PB8C1    DS    0H                                                               
         GOTO1 SEQ                                                              
PB8C2    DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   PB8D                                                             
         BAS   RE,PBSRCH                                                        
         B     PB8C1                                                            
*                                                                               
PB8D     DS    0H                                                               
         XC    KEY,KEY                                                          
         XC    DOUBLE,DOUBLE                                                    
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'2A'         NEW CIRCULATION RECORD                       
         GOTO1 HIGH                                                             
         B     PB8D2                                                            
*                                                                               
PB8D1    DS    0H                                                               
         GOTO1 SEQ                                                              
PB8D2    DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   PB8E                                                             
         BAS   RE,PBSRCH                                                        
         B     PB8D1                                                            
*                                                                               
PB8E     DS    0H                                                               
         XC    KEY,KEY                                                          
         XC    DOUBLE,DOUBLE                                                    
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'2B'         SPACE RECORD                                 
         GOTO1 HIGH                                                             
         B     PB8E2                                                            
*                                                                               
PB8E1    DS    0H                                                               
         GOTO1 SEQ                                                              
PB8E2    DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   PB8F                11/8/95                                      
         BAS   RE,PBSRCH                                                        
         B     PB8E1                                                            
*                                                                               
PB8F     DS    0H                                                               
         XC    KEY,KEY                                                          
         XC    DOUBLE,DOUBLE                                                    
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'3C'         NEW PUB GROUP DIR ONLY REC                   
         GOTO1 HIGH                                                             
         B     PB8F2                                                            
*                                                                               
PB8F1    DS    0H                                                               
         GOTO1 SEQ                                                              
PB8F2    DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   PB8G                                                             
         BAS   RE,PBSRCH                                                        
         B     PB8F1                                                            
*                                                                               
PB8G     DS    0H                                                               
         XC    KEY,KEY                                                          
         XC    DOUBLE,DOUBLE                                                    
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'3F'         NEW PUB GROUP DIR ONLY REC                   
         GOTO1 HIGH                                                             
         B     PB8G2                                                            
*                                                                               
PB8G1    DS    0H                                                               
         GOTO1 SEQ                                                              
PB8G2    DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   PB9                                                              
         BAS   RE,PBSRCH                                                        
         B     PB8G1                                                            
*                                                                               
PB9      DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'17'         PUB LIST RECORD                              
         GOTO1 HIGH                                                             
         B     PB9D                                                             
*                                                                               
PB9B     DS    0H                                                               
         GOTO1 SEQ                                                              
PB9D     DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   PB10                                                             
*                                                                               
         LA    RF,PCONREC          READ INTO CONREC AREA                        
         ST    RF,AREC                                                          
         GOTO1 GETPRT                                                           
         LA    R2,PCONREC+33                                                    
         MVI   ELCODE,X'20'        LIST PUB ELEMENT                             
         CLI   0(R2),X'20'                                                      
         BE    PB9H                                                             
PB9F     BAS   RE,NEXTEL                                                        
         BNE   PB9B                NEXT PUB LIST RECORD                         
PB9H     BAS   RE,PBSRCH                                                        
         B     PB9F                NEXT ELEMENT                                 
*                                                                               
PB10     DS    0H                                                               
         XC    KEY,KEY                                                          
         XC    DOUBLE,DOUBLE                                                    
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'15'         JOB RECORD                                   
         GOTO1 HIGH                                                             
         B     PB10D                                                            
*                                                                               
PB10B    DS    0H                                                               
         GOTO1 SEQ                                                              
PB10D    DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   PB99                DONE                                         
         BAS   RE,PBSRCH                                                        
         B     PB10B                                                            
*                                                                               
PB99     DS    0H                                                               
         GOTO1 =A(RESULTS)                                                      
         XMOD1 1                   RETURN                                       
*                                                                               
*                                                                               
PBSRCH   NTR1                      MOVE PUB CODE INTO DOUBLE                    
         XC    DOUBLE,DOUBLE                                                    
         MVC   DOUBLE(6),KEY+7                                                  
         CLI   KEY+3,X'10'         SEE IF CONTRACT                              
         BE    PBSRCH2                                                          
*                                                                               
         CLI   KEY+3,X'3C'         OR 3C PUB GRP DIR ONLY REC                   
         BE    PBSRCH2                                                          
*                                                                               
         MVC   DOUBLE(6),KEY+10                                                 
         CLI   KEY+3,X'3F'         3F PUB GRP DIR ONLY REC ?                    
         BE    PBSRCH2                                                          
*                                                                               
         MVC   DOUBLE(6),KEY+16                                                 
         CLI   KEY+3,X'15'         SEE IF JOB RECORD                            
         BE    PBSRCH2                                                          
*                                                                               
         MVC   DOUBLE(6),KEY+4     FOR MOST OTHER RECORDS                       
*                                                                               
         CLI   KEY+3,X'17'         SEE IF PUB LIST                              
         BNE   PBSRCH2                                                          
         MVC   DOUBLE(6),2(R2)     PUB CODE FROM PUB LIST                       
*                                                                               
PBSRCH2  MVI   DOUBLE+7,0                                                       
         GOTO1 BINSRCH,BSPARS,DOUBLE                                            
*                                                                               
         CLI   BSPARS,1            PUB NOT IN TABLE                             
         BE    PBEXIT                                                           
         L     RF,BSPARS                                                        
         CLI   7(RF),X'FF'         PUB CAN'T BE PURGED ANYWAY                   
         BE    PBEX8               STILL SHOW KEY                               
         CLI   KEY+3,X'17'         SEE IF PUB LIST                              
         BE    PBEX5               YES - NO DELETE                              
*                                                                               
         CLI   QOPT3,C'C'          SHOULD CONTRACT PREVENT PURGE?               
         BNE   PBSRCH2C                                                         
         CLI   KEY+3,X'10'         CONTRACT FOUND                               
         BE    PBEX5               PREVENT PURGE                                
*                                                                               
PBSRCH2C DS    0H                                                               
         CLI   QOPT5,C'Y'          RECORDS BEING DELETED?                       
         BNE   PBEX5               NO                                           
         OI    KEY+25,X'80'                                                     
         GOTO1 WRT                                                              
         MVC   PSECOND(11),=C'**DELETED**'                                      
         B     PBEX8                                                            
*                                                                               
PBEX5    MVI   7(RF),X'FF'  DON'T PURGE PUB - SOME RECORD FOUND                 
PBEX8    GOTO1 =A(PRNTKEY)                                                      
PBEXIT   XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
*                             ************************************              
*                             * CHECK PUB FILE FOR ACTIVE CLIENT *              
*                             *  FOR THESE ELEMENTS              *              
*                             *  X'11',X'14',X'08',X'09' AND     *              
*                             *  X'0A' ARE CHECKED FOR REGULAR   *              
*                             *  PUB RECORD.  ELEMENT X'71' IS   *              
*                             *  CHECKED FOR PUB LITTLE RECORD.  *              
*                             *  PUB ADDRESS RECORDS ARE ALSO    *              
*                             *  CHECKED FOR ACTIVE CLIENT.      *              
*                             ************************************              
CKPUB    CSECT                                                                  
         NMOD1 0,CKPUB                                                          
*                                                                               
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
*                                                                               
         MVI   WRTPUBSW,0                                                       
*                                                                               
         L     R3,ABSTAB      CLIENT TABLE                                      
         L     R4,BSPARS+8    # OF RECS IN THE CLIENT TABLE                     
CKPUB1   CLI   6(R3),X'FF'    ACTIVE CLIENT                                     
         BNE   CKPUBN                                                           
CKPUB2   LA    R3,8(R3)       NEXT CLIENT                                       
         BCT   R4,CKPUB1                                                        
*                                                                               
*                                                                               
         TM    WRTPUBSW,1     SEE IF I NEED TO WRITE BAC "BIG" PUB              
         BNO   CKPUB5                                                           
         LA    RE,MYIO         SAVE THE UPDATED RECORD IN MYIO                  
         LH    RF,=H'4000'                                                      
         XCEF                                                                   
         LA    RE,PUBREC                                                        
         LH    RF,=H'4000'                                                      
         LA    R0,MYIO                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         MVC   KEY,SVPUBKEY     MUST REREAD THE PUB                             
         GOTO1 HIGHPUB                                                          
         GOTO1 GETNAME                                                          
         LA    RE,MYIO          NOW RESTORE THE UPDATED PUB                     
         LH    RF,=H'4000'                                                      
         LA    R0,PUBREC                                                        
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,PUBREC                                                        
         ST    R0,AREC                                                          
         GOTO1 PUTPUB                                                           
*                                                                               
CKPUB5   TM    WRTPUBSW,X'02' SEE IF I NEED TO WRITE BACK "LITTLE" REC          
         BNO   CKPUBX                                                           
         LA    RE,MYIO                                                          
         LH    RF,=H'4000'                                                      
         XCEF                                                                   
         L     RE,ALTLREC       SAVE THE UPDATED RECORD IN MYIO                 
         LH    RF,=H'4000'                                                      
         LA    R0,MYIO                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         MVC   KEY,SVLTLKEY                                                     
         GOTO1 HIGHPUB                                                          
         GOTO1 GETLTL            MUST REREAD THE LITTLE RECORD                  
         LA    RE,MYIO                                                          
         LH    RF,=H'4000'                                                      
         LA    R0,PUBREC          MUST MOVE TO PUBREC TO USE PUTPUB             
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,PUBREC                                                        
         ST    R0,AREC                                                          
         GOTO1 PUTPUB                                                           
*                                                                               
*                                                                               
CKPUBX   DS    0H                                                               
         XMOD1 1              RETURN                                            
*                                                                               
*                                                                               
CKPUBN   EQU   *              PROCESS PUB NORMAL REC                            
         LA    R6,ELTAB       TABLE OF ELEMENTS TO CHECK                        
CKPUBNO  CLI   0(R6),X'FF'    SEE IF AT END                                     
         BE    CKPUBN30       GO CHECK FOR PREMIUM ELEMENTS                     
         LA    R5,PUBREC+33   ADDRESS OF THE FIRST ELEMENT                      
CKPUBN0  CLC   0(1,R5),0(R6)                                                    
         BNE   CKNEXTEL                                                         
         CLC   2(3,R5),0(R3)     SEE IF MATCHES CLIENT                          
         BNE   CKNEXTEL                                                         
         MVC   ELHOLD(3),1(R6)                                                  
         CLI   QOPT5,C'Y'        SEE IF DELETING CLIENT'S RECORDS               
         BNE   CKPUBN5           AND PUB ELEMENTS                               
         GOTO1 RECUP,DMCB,(1,PUBREC),0(R5),0                                    
*                                CLEAR END OF RECORD                            
         MVC   HALF,PUBREC+25                                                   
         LH    R1,HALF                                                          
         LA    RE,PUBREC                                                        
         AR    RE,R1                                                            
         SR    RF,RF                                                            
         LA    RF,PUBREC                                                        
         LA    RF,1999(RF)                                                      
         LA    RF,2000(RF)                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         MVC   PSECOND(19),=C'**ELEMENT DELETED**'                              
         OI    WRTPUBSW,1      SO I'LL KNOW TO WRITE RECORD BACK                
CKPUBN5  GOTO1 =A(PRNTKEY)                                                      
         CLI   QOPT5,C'Y'                                                       
         BE    CKPUBN20                                                         
         MVI   7(R3),X'FF'     SET CLIENT ACTIVE IN PUBFILE                     
         B     CKPUBN20        IF NOT PURGING THE CLIENTS ELEMENTS              
*                              (QOPT5=Y)                                        
CKNEXTEL DS    0H                                                               
         ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),0        SEE IF AT END OF RECORD                           
         BNE   CKPUBN0                                                          
CKPUBN20 LA    R6,4(R6)       NOW DO NEXT ELEMENT CODE FROM TABLE               
         B     CKPUBNO                                                          
*                                                                               
*                        NOW CHECK FOR CLIENT-SPECIFIC PREMIUM ELEMENTS         
CKPUBN30 DS    0H                                                               
         LA    R5,PUBREC+33        ADDRESS OF THE FIRST ELEMENT                 
CKPUBN35 CLI   0(R5),X'60'         PREMIUM ELEMENT ?                            
         BE    CKPUBN40                                                         
CKPUBN37 ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),0             SEE IF AT END OF RECORD                      
         BE    CKPUBNX             DONE - GO CHECK LITTLE REC                   
         B     CKPUBN35                                                         
CKPUBN40 DS    0H                                                               
         CLC   27(3,R5),0(R3)      SEE IF MATCHES CLIENT                        
         BNE   CKPUBN37                                                         
         MVC   ELHOLD(3),=C'PRM'                                                
         MVI   ELHOLD+3,C','                                                    
         MVC   ELHOLD+4(2),2(R5)   PREMIUM TYPE (1C,2C,ETC.)                    
         MVI   ELHOLD+6,C','                                                    
         GOTO1 DATCON,DMCB,(3,5(R5)),(5,ELHOLD+7)    PRM EFFECTIVE DATE         
         CLI   QOPT5,C'Y'        SEE IF DELETING CLIENT'S RECORDS               
         BNE   CKPUBN50          AND PUB ELEMENTS                               
CKPUBN42 GOTO1 RECUP,DMCB,(1,PUBREC),0(R5),0                                    
         CLI   0(R5),X'61'                                                      
         BE    CKPUBN42                                                         
*                                CLEAR END OF RECORD                            
         MVC   HALF,PUBREC+25                                                   
         LH    R1,HALF                                                          
         LA    RE,PUBREC                                                        
         AR    RE,R1                                                            
         SR    RF,RF                                                            
         LA    RF,PUBREC                                                        
         LA    RF,1999(RF)                                                      
         LA    RF,2000(RF)                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         MVC   PSECOND(19),=C'**ELEMENT DELETED**'                              
         OI    WRTPUBSW,1      SO I'LL KNOW TO WRITE RECORD BACK                
CKPUBN50 GOTO1 =A(PRNTKEY)                                                      
         CLI   QOPT5,C'Y'                                                       
         BE    CKPUBN35                                                         
         MVI   7(R3),X'FF'     SET CLIENT ACTIVE IN PUBFILE                     
         B     CKPUBN37        IF NOT PURGING THE CLIENTS ELEMENTS              
*                              (QOPT5=Y)                                        
*                                                                               
CKPUBNX  DS    0H                                                               
         B     CKPUBL                                                           
*                                                                               
ELTAB    DC    X'11',C'SUP'                                                     
         DC    X'14',C'REP'                                                     
         DC    X'08',C'PAY'                                                     
         DC    X'09',C'TRA'                                                     
         DC    X'0A',C'CON'                                                     
         DC    X'FF'                                                            
*                                                                               
*                                                                               
CKPUBL   DS    0H             PROCESS PUB LITTLE RECORD                         
         L     R5,ALTLREC                                                       
         LA    R5,33(R5)                                                        
CKPUBL0  CLI   0(R5),X'71'    PUB REG/DIST ELEMENT                              
         BNE   CKPUBL1                                                          
         CLC   2(3,R5),0(R3)  MATCH CLIENT                                      
         BNE   CKPUBL1                                                          
         MVC   ELHOLD(3),=C'DRD'                                                
         MVC   ELHOLD+3(9),5(R5)      SAVE DIV/REG/DST                          
         CLI   QOPT5,C'Y'                                                       
         BNE   CKPUBL6                                                          
         OI    WRTPUBSW,X'02'                                                   
         MVC   PSECOND(19),=C'**ELEMENT DELETED**'                              
         L     R6,ALTLREC                                                       
         GOTO1 RECUP,DMCB,(1,0(R6)),0(R5),0                                     
*                                CLEAR END OF RECORD                            
         MVC   HALF,25(R6)                                                      
         LH    R1,HALF                                                          
         LR    RE,R6                                                            
         AR    RE,R1                                                            
         LR    RF,R6                                                            
         LA    RF,1999(RF)                                                      
         LA    RF,2000(RF)                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
CKPUBL5  GOTO1 =A(PRNTKEY)                                                      
         B     CKPUBL0        PRINT AND PROCESS NEXT ELEMENT                    
*                                                                               
CKPUBL6  GOTO1 =A(PRNTKEY)                                                      
         MVI   7(R3),X'FF'     SET CLIENT ACTIVE IN PUBFILE                     
         B     CKPUBL1         WHEN NOT PURGING THE CLIENT ELEMENTS             
*                              (QOPT5=Y)                                        
CKPUBL1  ZIC   R0,1(R5)                                                         
         AR    R5,R0          ADDR OF THE BEGINNING OF NEXT ELEM                
         CLI   0(R5),0        END OF PUB LITTLE REC                             
         BE    CKPUBAD        PROCESS PUB ADDRESS RECORDS                       
         B     CKPUBL0        CHECK NEXT ELEMENT                                
*                                                                               
*                                                                               
CKPUBAD  DS    0H             PROCESS PUB ADDRESS RECORDS                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(9),PUBKEY                                                    
         MVI   KEY+9,X'82'         PUB ADDRESS RECORDS                          
         GOTO1 HIGHPUB                                                          
         B     CKPUBA3                                                          
CKPUBA2  GOTO1 SEQPUB                                                           
CKPUBA3  CLC   KEY(10),KEYSAVE     CHECK THRU RECORD CODE                       
         BNE   CKPUB2              PROCESS NEXT CLIENT                          
         TM    KEY+25,X'80'        DELETED ?                                    
         BO    CKPUBA2             YES - NEXT RECORD                            
         CLC   KEY+11(3),0(R3)     CLIENT MATCH ?                               
         BNE   CKPUBA2             NO - NEXT RECORD                             
         CLI   QOPT5,C'Y'          DELETING CLIENT'S RECORDS ?                  
         BNE   CKPUBA4             NO                                           
         OI    KEY+25,X'80'                                                     
         GOTO1 WRTPUB                                                           
CKPUBA4  MVC   ELHOLD(3),=C'PAY'                                                
         CLI   KEY+10,X'08'        PAYING ADDRESS ?                             
         BE    CKPUBA5             YES                                          
         MVC   ELHOLD(3),=C'TRA'                                                
         CLI   KEY+10,X'09'        TRAFFIC ADDRESS ?                            
         BE    CKPUBA5             YES                                          
         MVC   ELHOLD(3),=C'CON'                                                
         CLI   KEY+10,X'0A'        CONTRACT ADDRESS ?                           
         BE    CKPUBA5             YES                                          
         MVC   ELHOLD(3),=C'SHI'                                                
         CLI   KEY+10,X'0B'        SHIPPING ADDRESS ?                           
         BE    CKPUBA5             YES                                          
         MVC   ELHOLD(3),=C'???'                                                
CKPUBA5  CLI   QOPT5,C'Y'          DELETING CLIENT'S RECORDS ?                  
         BNE   CKPUBA7             NO                                           
         MVC   PSECOND(23),=C'**PUB ADDRESS DELETED**'                          
*                                                                               
CKPUBA7  GOTO1 =A(PRNTKEY)                                                      
         CLI   QOPT5,C'Y'          PURGING CLIENT'S RECORDS ?                   
         BE    CKPUBA2             YES - PROCESS NEXT RECORD                    
         MVI   7(R3),X'FF'     SET CLIENT ACTIVE IN PUBFILE                     
         B     CKPUBA2         WHEN NOT PURGING THE CLIENT RECORDS              
*                              (QOPT5=Y)                                        
         LTORG                                                                  
MYIO     DS    4000C                                                            
*                                                                               
         DROP  R7                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
*        PRINT KEYS                                                             
*                                                                               
***********************************************************************         
*                                                                               
PRNTKEY  CSECT                                                                  
         NMOD1 0,PRNTKEY                                                        
*                                                                               
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
*                                                                               
         LA    R7,1(RB)                                                         
         LA    R7,4095(R7)                                                      
         USING PRNTKEY+4096,R7     SECOND BASE REGISTER                         
*                                                                               
         CLI   QOPT4,C'Y'                                                       
         BE    PK1X                                                             
         CLI   QOPT2,C'G'      PURGEABLE RECORDS                                
         BE    PK1                                                              
         CLI   QOPT2,C'A'                                                       
         BE    PRTXIT                                                           
         B     PK1X                                                             
*                                                                               
*        SPECIAL FOR PROCESSING PURGABLE RECORDS ONLY                           
*                                                                               
PK1      DS    0H                                                               
         L     R1,BSPARS      BASE RECORD IN BINSRCH TABLE                      
         CLI   QOPT1,C'P'     SEE PURGING PUBS                                  
         BNE   PK1A                                                             
         CLI   7(R1),0        IS PUB PURGEABLE?                                 
         BNE   PRTXIT         NO THEN SKIP PRINTING KEY                         
         B     PK1X                                                             
*                                                                               
PK1A     CLI   QOPT1,C'H'     SEE PURGING CLIENTS                               
         BNE   PK1B                                                             
         CLC   6(2,R1),X'0000'   IS CLIENT PURGEABLE?                           
         BNE   PRTXIT         NO THEN SKIP PRINTING KEY                         
         B     PK1X                                                             
*                                                                               
PK1B     CLI   QOPT1,C'R'     SEE PURGING REPS                                  
         BNE   PK1C                                                             
         CLI   5(R1),0         IS REP PURGEABLE?                                
         BNE   PRTXIT         NO THEN SKIP PRINTING KEY                         
         B     PK1X                                                             
*                                                                               
PK1C     CLI   QOPT1,C'J'     SEE PURGING JOBS                                  
         BNE   PK1D                                                             
         CLI   12(R1),0        IS JOB PURGEABLE?                                
         BNE   PRTXIT         NO THEN SKIP PRINTING KEY                         
         B     PK1X                                                             
*                                                                               
PK1D     CLI   QOPT1,C'C'     SEE PURGING COMMENTS                              
         BNE   PK1E                                                             
         CLI   6(R1),0        IS COMMENT PURGEABLE?                             
         BNE   PRTXIT         NO THEN SKIP PRINTING KEY                         
         B     PK1X                                                             
*                                                                               
PK1E     CLI   QOPT1,C'L'     SEE PURGING PUBLISTS                              
         BNE   PK1X                                                             
         CLI   6(R1),0        IS PUBLIST PURGEABLE?                             
         BNE   PRTXIT         NO THEN SKIP PRINTING KEY                         
         B     PK1X                                                             
*                                                                               
PK1X     CLI   KEY+1,C'A'                                                       
         BL    PKPUB                                                            
         CLI   KEY+3,X'20'                                                      
         BE    PKBUY                                                            
         CLI   KEY+3,X'21'                                                      
         BE    PKBUY2                                                           
         CLI   KEY+3,X'10'                                                      
         BE    PKCON                                                            
         CLI   KEY+3,X'14'        AOR                                           
         BE    PKAOR                                                            
         CLI   KEY+3,X'15'                                                      
         BE    PKJOB                                                            
         CLI   KEY+3,X'25'        CLEARANCE STATUS                              
         BE    PKCLEAR                                                          
         CLI   KEY+3,X'26'                                                      
         BE    PKPGCOST                                                         
         CLI   KEY+3,X'27'                                                      
         BE    PKFSI                                                            
         CLI   KEY+3,X'28'                                                      
         BE    PKSCU               SPACE CONTRACT UNIT                          
         CLI   KEY+3,X'29'                                                      
         BE    PKISS                                                            
         CLI   KEY+3,X'2A'                                                      
         BE    PKCRC                                                            
         CLI   KEY+3,X'2B'                                                      
         BE    PKSPC                                                            
         CLI   KEY+3,X'3A'         CLIENT GROUP DIR REC                         
         BE    PKCGDA                                                           
         CLI   KEY+3,X'3D'         CLIENT GROUP DIR REC                         
         BE    PKCGDD                                                           
         CLI   KEY+3,X'3C'         PUB GROUP DIR REC                            
         BE    PKPGDC                                                           
         CLI   KEY+3,X'3F'         PUB GROUP DIR REC                            
         BE    PKPGDF                                                           
         CLI   KEY+3,X'02'                                                      
         BE    PKCLT                                                            
         CLI   KEY+3,X'06'                                                      
         BE    PKPRD                                                            
         CLI   KEY+3,X'07'                                                      
         BE    PKEST                                                            
         CLI   KEY+3,X'0A'                                                      
         BE    PKESTPG                                                          
         CLI   KEY+3,X'0B'                                                      
         BE    PKESTGF                                                          
         CLI   KEY+3,X'03'        DIVISIONS                                     
         BE    PKDIV                                                            
         CLI   KEY+3,X'04'        REGIONS                                       
         BE    PKREG                                                            
         CLI   KEY+3,X'05'        DISTRICTS                                     
         BE    PKDST                                                            
         CLI   KEY+3,X'09'        EST BUCKETS                                   
         BE    PKBUC                                                            
         CLI   KEY+3,X'08'        BILLRESC                                      
         BE    PKBIL                                                            
         CLI   KEY+3,X'17'        PUB LISTS                                     
         BE    PKPLT                                                            
         CLI   KEY+3,X'18'        BUDGET RECORDS                                
         BE    PKBUD                                                            
         CLI   KEY+3,X'30'        USERP RECS                                    
         BE    PKUSR                                                            
         CLI   KEY+3,X'41'        NV TEST RECS                                  
         BE    PKNVT                                                            
         CLI   KEY+3,X'4C'        BILL FORMULA RECORDS                          
         BE    PKBLF                                                            
         CLI   KEY+3,X'50'        INV MATCHING                                  
         BE    PKINVM                                                           
         CLI   KEY+3,X'90'        PUB/EST UPLOAD                                
         BE    PKPEU                                                            
         B     PRTXIT                                                           
*                                                                               
PK2      DS    0H                                                               
         CLI   QOPT1,C'P'                                                       
         BE    PK20                                                             
         CLI   QOPT1,C'R'          REPS                                         
         BNE   PK4                                                              
         L     RF,BSPARS                                                        
         CLI   MISSSW,C'Y'                                                      
         BE    PK2B                                                             
         MVC   P+50(4),=C'USES'                                                 
         MVC   P+55(4),0(RF)                                                    
         B     PK2D                                                             
*                                                                               
PK2B     MVC   P+22(7),=C'MISSING'                                              
         MVC   P+30(4),0(RF)                                                    
PK2D     CLI   QMEDIA,C'O'         CHK OUTDOOR                                  
         BNE   PK20                                                             
*              R4 FROM BASE PROGRAM                                             
         CHI   R4,2                SEE IF DOING TRAFFIC REP                     
         BNE   PK20                NO                                           
         CLI   4(RF),0             SEE IF DOING SUFFIX                          
         BE    PK20                NO                                           
         MVI   P+34,C'.'                                                        
         MVC   P+35(1),4(RF)       SUFFIX                                       
         B     PK20                                                             
*                                                                               
PK4      DS    0H                                                               
         CLI   QOPT1,C'J'                                                       
         BNE   PK6                                                              
         L     RF,BSPARS                                                        
         MVC   P+49(4),=C'USES'                                                 
         MVC   P+54(6),6(RF)                                                    
         B     PK20                                                             
PK6      DS    0H                                                               
         CLI   QOPT1,C'C'                                                       
         BNE   PK20                                                             
         L     RF,BSPARS                                                        
         CLI   MISSSW,C'Y'                                                      
         BE    PK6C                                                             
         MVC   P+49(4),=C'USES'                                                 
         MVC   P+54(6),0(RF)                                                    
         B     PK20                                                             
*                                                                               
PK6C     MVC   P+49(6),0(RF)                                                    
         MVC   P+57(7),=C'MISSING'                                              
         B     PK20                                                             
*                                                                               
PK20     DS    0H                                                               
         BAS   RE,RPRT                                                          
         B     PRTXIT                                                           
*                                                                               
*                                                                               
*                                                                               
PKBUY    DS    0H                                                               
*                                                                               
         MVC   WORK(25),KEY                                                     
         B     PKBUY4                                                           
*                                                                               
*                                                                               
*                                                                               
PKBUY2   DS    0H                                                               
         MVC   WORK(7),KEY                                                      
         MVC   WORK+7(3),KEY+13                                                 
         MVC   WORK+10(6),KEY+7                                                 
         MVC   WORK+16(9),KEY+16                                                
*                                                                               
PKBUY4   DS    0H                                                               
*                                                                               
         MVC   SVWORK,WORK         NEED TO SAVE OFF WORK                        
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKBUY010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'BUY'                                                   
         BE    PKBUY090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKBUY010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4     SO SUBHEADING WON'T PRINT BY ITSELF               
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(03),=C'CLT'                                                 
         MVC   P+09(03),=C'PRD'                                                 
         MVC   P+13(03),=C'EST'                                                 
         MVC   P+17(03),=C'PUB'                                                 
         MVC   P+35(14),=C'INSERTION DATE'                                      
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(03),=C'---'                                                 
         MVC   P+09(03),=C'---'                                                 
         MVC   P+13(03),=C'---'                                                 
         MVC   P+17(03),=C'---'                                                 
         MVC   P+35(14),=C'--------------'                                      
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'BUY'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKBUY090 DS    0H                                                               
         MVC   WORK,SVWORK         RESTORING WORK                               
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         MVC   P(3),=C'BUY'                                                     
         MVC   P+5(3),WORK+4                                                    
         MVC   P+9(3),WORK+7                                                    
         MVC   HALF,WORK+19                                                     
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+13(3),DUB                                                      
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),WORK+10),P+17                                 
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK+16),(5,P+35)                                 
*                                                                               
         CLI   WORK+24,1                                                        
         BE    PKBUY6                                                           
         SR    R0,R0                                                            
         IC    R0,WORK+24                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+44(2),DUB                                                      
         MVI   P+43,C'-'                                                        
*                                                                               
PKBUY6   DS    0H                                                               
         CLI   QOPT1,C'L'          PUB LIST RECORD PURGE ?                      
         BNE   PK2                 NO                                           
         MVC   P+50(3),WORK+50     SHOULD CONTAIN PUB LIST CODE                 
         B     PK2                                                              
*                                                                               
*                                                                               
*                                                                               
PKPUB    DS    0H                                                               
*                                                                               
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKPUB010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'PUB'                                                   
         BE    PKPUB090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKPUB010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4     SO SUBHEADING WON'T PRINT BY ITSELF               
         CLI   QOPT1,C'H'      SEE IF PROCESSING CLIENTS                        
         BNE   PKPUB012                                                         
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(03),=C'CLT'                                                 
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(03),=C'---'                                                 
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'PUB'      SET FLAG TO CURRENT REC TYPE                 
         B     PKPUB090                                                         
*                                                                               
PKPUB012 DS    0H                                                               
         MVC   P+00(03),=C'PUB'                                                 
         MVC   P+20(03),=C'CLT'                                                 
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+20(03),=C'---'                                                 
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'PUB'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKPUB090 DS    0H                                                               
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         CLI   QOPT1,C'H'                                                       
         BNE   PKPUB10                                                          
         MVC   P(3),=C'PUB'                                                     
         MVC   P+5(3),0(R3)        MVC CLIENT                                   
         CLC   ELHOLD(3),=C'PRM'                                                
         BNE   PKPUB1                                                           
         MVC   P+9(15),ELHOLD      "PRM,NC,MM/DD/YY"                            
         B     PKPUB3                                                           
PKPUB1   MVC   P+9(3),ELHOLD       ONE OF 'SUP,REP,PAY,TRA,CON,DRD'             
         CLC   ELHOLD(3),=C'DRD'                                                
         BNE   PKPUB5                                                           
         MVC   P+13(3),ELHOLD+3                                                 
         MVI   P+16,C'/'                                                        
         MVC   P+17(3),ELHOLD+6                                                 
         CLI   ELHOLD+9,C'0'        SEE IF THERE IS A DISTRICT                  
         BL    PKPUB3                                                           
         MVI   P+20,C'/'                                                        
         MVC   P+21(3),ELHOLD+9                                                 
PKPUB3   IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),KEY+1),P+26                                   
         B     PK20                                                             
*                                                                               
PKPUB5   IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),KEY+1),P+17                                   
         B     PK20                                                             
*                                                                               
PKPUB10  IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),KEY+1),P                                      
         CLI   QOPT1,C'R'          SEE IF PURGING REPS                          
         BE    PKPUB15                                                          
         CLI   QOPT1,C'C'          OR IF PURGING COMMENTS                       
         BNE   PK2                                                              
*                                                                               
PKPUB15  DS    0H                                                               
         CLI   PUBLSW,C'Y'                                                      
         BNE   PKPUB20                                                          
         MVC   P+20(13),=C'PUBLISHER REP'                                       
         B     PK2                                                              
*                                                                               
PKPUB20  MVC   P+20(3),2(R2)       CLIENT CODE FROM CLIENT ELEM                 
         CLI   2(R2),X'FF'                                                      
         BNE   *+8                                                              
         MVI   P+20,C'*'                                                        
         CLC   2(3,R2),=X'FFFFFF'                                               
         BNE   *+10                                                             
         MVC   P+20(3),=C'ALL'                                                  
         B     PK2                                                              
*                                                                               
*                                                                               
*                                                                               
PKCON    DS    0H                                                               
*                                                                               
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKCON010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'CON'                                                   
         BE    PKCON090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKCON010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4     SO SUBHEADING WON'T PRINT BY ITSELF               
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(03),=C'CLT'                                                 
         MVC   P+09(03),=C'PUB'                                                 
         MVC   P+26(15),=C'CONTRACT NUMBER'                                     
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(03),=C'---'                                                 
         MVC   P+09(03),=C'---'                                                 
         MVC   P+26(15),=C'---------------'                                     
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'CON'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKCON090 DS    0H                                                               
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         MVC   P(3),=C'CON'                                                     
         MVC   P+5(3),KEY+4                                                     
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),KEY+7),P+9                                    
*                                                                               
         MVC   HALF,KEY+13                                                      
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+26(3),DUB                                                      
*                                                                               
         B     PK2                                                              
*                                                                               
*                                                                               
*                                                                               
PKCLEAR  DS    0H                                                               
*                                                                               
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKCLR010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'CLR'                                                   
         BE    PKCLR090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKCLR010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4     SO SUBHEADING WON'T PRINT BY ITSELF               
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(03),=C'CLT'                                                 
         MVC   P+09(03),=C'PUB'                                                 
         MVC   P+30(08),=C'CLR DATE'                                            
         MVC   P+40(15),=C'SEQUENCE NUMBER'                                     
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(03),=C'---'                                                 
         MVC   P+09(03),=C'---'                                                 
         MVC   P+30(08),=C'--------'                                            
         MVC   P+40(15),=C'---------------'                                     
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'CLR'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKCLR090 DS    0H                                                               
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         MVC   P(3),=C'CLR'                                                     
         MVC   P+5(3),KEY+4                                                     
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),KEY+7),P+9                                    
*                                                                               
         GOTO1 DATCON,DMCB,(3,KEY+13),(5,P+30)                                  
*                                                                               
         ZIC   R0,KEY+16              SEQUENCE NUMBER                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+40(3),DUB                                                      
*                                                                               
         B     PK2                                                              
*                                                                               
*                                                                               
*                                                                               
PKJOB    DS    0H                                                               
*                                                                               
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKJOB010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'JOB'                                                   
         BE    PKJOB090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKJOB010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4     SO SUBHEADING WON'T PRINT BY ITSELF               
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(03),=C'CLT'                                                 
         MVC   P+09(03),=C'PRD'                                                 
         MVC   P+13(08),=C'JOB CODE'                                            
         MVC   P+25(03),=C'PUB'                                                 
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(03),=C'---'                                                 
         MVC   P+09(03),=C'---'                                                 
         MVC   P+13(08),=C'--------'                                            
         MVC   P+25(03),=C'---'                                                 
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'JOB'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKJOB090 DS    0H                                                               
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         MVC   P(3),=C'JOB'                                                     
         MVC   P+5(3),KEY+4                                                     
         MVC   P+9(3),KEY+7                                                     
         MVC   P+13(6),KEY+10                                                   
         OC    KEY+16(6),KEY+16                                                 
         BZ    PKJOB2                                                           
         MVC   P+25(8),=C'ALL PUBS'                                             
         CLI   KEY+16,X'FF'                                                     
         BE    PKJOB2                                                           
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),KEY+16),P+25                                  
*                                                                               
PKJOB2   DS    0H                                                               
         CLI   QOPT1,C'L'          PUB LIST RECORD PURGE ?                      
         BNE   PK2                 NO                                           
         MVC   P+50(3),WORK+50     SHOULD CONTAIN PUB LIST CODE                 
         B     PK2                                                              
*                                                                               
*                                                                               
*                                                                               
PKFSI    DS    0H                                                               
*                                                                               
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKFSI010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'FSI'                                                   
         BE    PKFSI090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKFSI010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4     SO SUBHEADING WON'T PRINT BY ITSELF               
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(03),=C'CLT'                                                 
         MVC   P+09(03),=C'PUB'                                                 
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(03),=C'---'                                                 
         MVC   P+09(03),=C'---'                                                 
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'FSI'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKFSI090 DS    0H                                                               
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         MVC   P(3),=C'FSI'                                                     
         MVC   P+5(3),KEY+10       CLIENT                                       
         MVC   P+9(8),=C'ALL PUBS'                                              
         CLI   KEY+8,X'FF'                                                      
         BE    PKFSI2                                                           
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),KEY+4),P+9                                    
*                                                                               
PKFSI2   DS    0H                                                               
         B     PK2                                                              
*                                                                               
*                                                                               
*                                                                               
PKSCU    DS    0H                                                               
*                                                                               
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKSCU010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'SCU'                                                   
         BE    PKSCU090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKSCU010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4     SO SUBHEADING WON'T PRINT BY ITSELF               
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(03),=C'CLT'                                                 
         MVC   P+09(03),=C'PUB'                                                 
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(03),=C'---'                                                 
         MVC   P+09(03),=C'---'                                                 
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'SCU'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKSCU090 DS    0H                                                               
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         MVC   P(3),=C'SCU'                                                     
         MVC   P+5(3),KEY+10       CLIENT                                       
         MVC   P+9(8),=C'ALL PUBS'                                              
         CLI   KEY+8,X'FF'                                                      
         BE    PKSCU2                                                           
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),KEY+4),P+9                                    
*                                                                               
PKSCU2   DS    0H                                                               
         B     PK2                                                              
*                                                                               
*                                                                               
*                                                                               
PKISS    DS    0H                                                               
*                                                                               
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKISS010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'ISS'                                                   
         BE    PKISS090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKISS010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4     SO SUBHEADING WON'T PRINT BY ITSELF               
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(10),=C'ISSUE YEAR'                                          
         MVC   P+17(03),=C'PUB'                                                 
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(10),=C'----------'                                          
         MVC   P+17(03),=C'---'                                                 
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'ISS'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKISS090 DS    0H                                                               
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         MVC   P(3),=C'ISS'                                                     
         MVC   P+5(4),KEY+10       ISSUE YEAR                                   
         MVC   P+17(8),=C'ALL PUBS'                                             
         CLI   KEY+8,X'FF'                                                      
         BE    PKISS2                                                           
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),KEY+4),P+17                                   
PKISS2   DS    0H                                                               
         B     PK2                                                              
*                                                                               
*                                                                               
*                                                                               
PKCRC    DS    0H                                                               
*                                                                               
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKCRC010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'CRC'                                                   
         BE    PKCRC090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKCRC010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4     SO SUBHEADING WON'T PRINT BY ITSELF               
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(03),=C'CLT'                                                 
         MVC   P+09(03),=C'PUB'                                                 
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(03),=C'---'                                                 
         MVC   P+09(03),=C'---'                                                 
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'CRC'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKCRC090 DS    0H                                                               
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         MVC   P(3),=C'CRC'                                                     
         MVC   P+5(3),KEY+17      CLIENT                                        
         MVC   P+9(8),=C'ALL PUBS'                                              
         CLI   KEY+8,X'FF'                                                      
         BE    PKCRC2                                                           
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),KEY+4),P+9                                    
PKCRC2   DS    0H                                                               
         B     PK2                                                              
*                                                                               
*                                                                               
*                                                                               
PKSPC    DS    0H                                                               
*                                                                               
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKSPC010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'SPC'                                                   
         BE    PKSPC090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKSPC010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4     SO SUBHEADING WON'T PRINT BY ITSELF               
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(03),=C'CLT'                                                 
         MVC   P+10(03),=C'PUB'                                                 
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(03),=C'---'                                                 
         MVC   P+10(03),=C'---'                                                 
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'SPC'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKSPC090 DS    0H                                                               
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         MVC   P(3),=C'SPC'                                                     
         MVC   P+5(3),KEY+10      CLIENT                                        
         MVC   P+10(8),=C'ALL PUBS'                                             
         CLI   KEY+8,X'FF'                                                      
         BE    PKSPC2                                                           
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),KEY+4),P+10                                   
PKSPC2   DS    0H                                                               
         B     PK2                                                              
*                                                                               
*                                                                               
*                                                                               
PKCGDA   DS    0H                                                               
*                                                                               
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKGRA010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'GRA'                                                   
         BE    PKGRA090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKGRA010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4     SO SUBHEADING WON'T PRINT BY ITSELF               
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(03),=C'CLT'                                                 
         MVC   P+09(06),=C'GRP ID'                                              
         MVC   P+16(08),=C'GRP CODE'                                            
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(03),=C'---'                                                 
         MVC   P+09(06),=C'------'                                              
         MVC   P+16(08),=C'--------'                                            
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'GRA'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKGRA090 DS    0H                                                               
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         MVC   P(3),=C'GRA'        CLIENT GROUP '3A' REC                        
         MVC   P+5(3),KEY+7        CLIENT                                       
         MVC   P+9(1),KEY+13       GROUP ID                                     
         MVC   WORK(2),KEY+14      GROUP CODE                                   
         MVI   WORK+2,X'0F'                                                     
         UNPK  DUB(5),WORK(3)                                                   
         MVC   P+16(4),DUB                                                      
         B     PK2                                                              
*                                                                               
*                                                                               
*                                                                               
PKCGDD   DS    0H                                                               
*                                                                               
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKGRD010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'GRD'                                                   
         BE    PKGRD090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKGRD010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4     SO SUBHEADING WON'T PRINT BY ITSELF               
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(03),=C'CLT'                                                 
         MVC   P+09(06),=C'GRP ID'                                              
         MVC   P+16(08),=C'GRP CODE'                                            
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(03),=C'---'                                                 
         MVC   P+09(06),=C'------'                                              
         MVC   P+16(08),=C'--------'                                            
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'GRD'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKGRD090 DS    0H                                                               
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         MVC   P(3),=C'GRD'        CLIENT GROUP '3D' REC                        
         MVC   P+5(3),KEY+10       CLIENT                                       
         MVC   P+9(1),KEY+7        GROUP ID                                     
         MVC   WORK(2),KEY+8       GROUP CODE                                   
         MVI   WORK+2,X'0F'                                                     
         UNPK  DUB(5),WORK(3)                                                   
         MVC   P+16(4),DUB                                                      
         B     PK2                                                              
*                                                                               
*                                                                               
*                                                                               
PKPGDC   DS    0H                                                               
*                                                                               
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKGRC010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'GRC'                                                   
         BE    PKGRC090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKGRC010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4     SO SUBHEADING WON'T PRINT BY ITSELF               
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(06),=C'GRP ID'                                              
         MVC   P+12(08),=C'GRP CODE'                                            
         MVC   P+21(03),=C'PUB'                                                 
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(06),=C'------'                                              
         MVC   P+12(08),=C'--------'                                            
         MVC   P+21(03),=C'---'                                                 
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'GRC'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKGRC090 DS    0H                                                               
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         MVC   P(3),=C'GRC'        PUB GROUP '3C' REC                           
         MVC   P+5(1),KEY+13       GROUP ID                                     
         MVC   WORK(2),KEY+14      GROUP CODE                                   
         MVI   WORK+2,X'0F'                                                     
         UNPK  DUB(5),WORK(3)                                                   
         MVC   P+12(4),DUB                                                      
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),KEY+7),P+21     PUB CODE                      
         B     PK2                                                              
*                                                                               
*                                                                               
*                                                                               
PKPGDF   DS    0H                                                               
*                                                                               
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKGRF010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'GRF'                                                   
         BE    PKGRF090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKGRF010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4     SO SUBHEADING WON'T PRINT BY ITSELF               
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(06),=C'GRP ID'                                              
         MVC   P+12(08),=C'GRP CODE'                                            
         MVC   P+21(03),=C'PUB'                                                 
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(06),=C'------'                                              
         MVC   P+12(08),=C'--------'                                            
         MVC   P+21(03),=C'---'                                                 
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'GRF'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKGRF090 DS    0H                                                               
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         MVC   P(3),=C'GRF'        PUB GROUP '3F' REC                           
         MVC   P+5(1),KEY+7        GROUP ID                                     
         MVC   WORK(2),KEY+8       GROUP CODE                                   
         MVI   WORK+2,X'0F'                                                     
         UNPK  DUB(5),WORK(3)                                                   
         MVC   P+12(4),DUB                                                      
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),KEY+10),P+21    PUB CODE                      
         B     PK2                                                              
*                                                                               
*                                                                               
*                                                                               
PKPEU    DS    0H                                                               
*                                                                               
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKPEU010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'PEU'                                                   
         BE    PKPEU090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKPEU010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4     SO SUBHEADING WON'T PRINT BY ITSELF               
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(03),=C'CLT'                                                 
         MVC   P+09(03),=C'PRD'                                                 
         MVC   P+13(03),=C'PUB'                                                 
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(03),=C'---'                                                 
         MVC   P+09(03),=C'---'                                                 
         MVC   P+13(03),=C'---'                                                 
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'PEU'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKPEU090 DS    0H                                                               
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         MVC   P(3),=C'PEU'                                                     
         MVC   P+5(3),KEY+4        CLIENT                                       
         MVC   P+9(3),KEY+7        PRODUCT                                      
         CLI   KEY+17,X'FF'                                                     
         BE    PKPEU2                                                           
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),KEY+12),P+13                                  
         B     PK2                                                              
*                                                                               
PKPEU2   MVC   P+13(3),=C'EDR'                                                  
         B     PK2                                                              
*                                                                               
*                                                                               
*                                                                               
PKPGCOST DS    0H                                                               
*                                                                               
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKPGC010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'PGC'                                                   
         BE    PKPGC090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKPGC010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4     SO SUBHEADING WON'T PRINT BY ITSELF               
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(03),=C'CLT'                                                 
         MVC   P+09(03),=C'PUB'                                                 
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(03),=C'---'                                                 
         MVC   P+09(03),=C'---'                                                 
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'PGC'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKPGC090 DS    0H                                                               
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         MVC   P(3),=C'PGC'                                                     
         MVC   P+5(3),KEY+4        CLIENT                                       
         CLC   KEY+7(6),=6X'FF'                                                 
         BNE   *+14                                                             
         MVC   P+9(3),=C'ALL'      ALL PUBS                                     
         B     PK2                                                              
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),KEY+7),P+9                                    
         B     PK2                                                              
*                                                                               
*                                                                               
*                                                                               
PKCLT    DS    0H                                                               
*                                                                               
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKCLT010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'CLT'                                                   
         BE    PKCLT090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKCLT010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4     SO SUBHEADING WON'T PRINT BY ITSELF               
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(03),=C'CLT'                                                 
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(03),=C'---'                                                 
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'CLT'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKCLT090 DS    0H                                                               
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         MVC   P(3),=C'CLT'                                                     
         MVC   P+5(3),KEY+4                                                     
         B     PK2                                                              
*                                                                               
*                                                                               
*                                                                               
PKPRD    DS    0H                                                               
*                                                                               
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKPRD010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'PRD'                                                   
         BE    PKPRD090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKPRD010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4                                                       
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(03),=C'CLT'                                                 
         MVC   P+09(03),=C'PRD'                                                 
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(03),=C'---'                                                 
         MVC   P+09(03),=C'---'                                                 
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'PRD'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKPRD090 DS    0H                                                               
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         MVC   P(3),=C'PRD'                                                     
         MVC   P+5(3),KEY+4                                                     
         MVC   P+9(3),KEY+7                                                     
         B     PK2                                                              
*                                                                               
*                                                                               
*                                                                               
PKUSR    DS    0H                                                               
*                                                                               
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKUSR010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'USR'                                                   
         BE    PKUSR090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKUSR010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4                                                       
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(03),=C'CLT'                                                 
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(03),=C'---'                                                 
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'USR'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKUSR090 DS    0H                                                               
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         MVC   P(3),=C'USR'                                                     
         MVC   P+5(4),KEY+4                                                     
         B     PK2                                                              
*                                                                               
*                                                                               
*                                                                               
PKNVT    DS    0H                                                               
*                                                                               
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKNVT010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'NVT'                                                   
         BE    PKNVT090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKNVT010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4                                                       
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(03),=C'CLT'                                                 
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(03),=C'---'                                                 
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'NVT'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKNVT090 DS    0H                                                               
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         MVC   P(3),=C'NVT'                                                     
         MVC   P+5(3),KEY+4                                                     
         B     PK2                                                              
*                                                                               
*                                                                               
*                                                                               
PKAOR    DS    0H                                                               
*                                                                               
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKAOR010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'AOR'                                                   
         BE    PKAOR090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKAOR010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4                                                       
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(03),=C'CLT'                                                 
         MVC   P+09(03),=C'PRD'                                                 
         MVC   P+13(03),=C'EST'                                                 
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(03),=C'---'                                                 
         MVC   P+09(03),=C'---'                                                 
         MVC   P+13(03),=C'---'                                                 
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'AOR'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKAOR090 DS    0H                                                               
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         MVC   P(3),=C'AOR'                                                     
         MVC   P+5(3),KEY+4                                                     
         MVC   P+9(3),KEY+7                                                     
         CLI   KEY+10,X'FF'                                                     
         BNE   PKAOR5                                                           
         MVC   P+13(3),=C'ALL'                                                  
         B     PK2                                                              
PKAOR5   ZICM  R0,KEY+10,2                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+13(3),DUB+6(2)        ESTIMATE                                 
         B     PK2                                                              
*                                                                               
*                                                                               
*                                                                               
PKPLT    DS    0H                                                               
*                                                                               
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKLST010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'LST'                                                   
         BE    PKLST090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKLST010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4                                                       
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(03),=C'CLT'                                                 
         MVC   P+09(03),=C'PRD'                                                 
         MVC   P+17(03),=C'PUB'                                                 
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(03),=C'---'                                                 
         MVC   P+09(03),=C'---'                                                 
         MVC   P+17(03),=C'---'                                                 
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'LST'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKLST090 DS    0H                                                               
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         MVC   P(3),=C'LST'        PUBLISTS                                     
         MVC   P+5(3),KEY+4                                                     
         MVC   P+9(3),KEY+7                                                     
         CLI   QOPT1,C'P'          PUB PURGE ?                                  
         BNE   PK2                 NO                                           
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),2(R2)),P+17                                   
         B     PK2                                                              
*                                                                               
*                                                                               
*                                                                               
PKDIV    DS    0H                                                               
*                                                                               
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKDIV010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'DIV'                                                   
         BE    PKDIV090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKDIV010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4                                                       
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(03),=C'CLT'                                                 
         MVC   P+09(08),=C'DIVISION'                                            
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(03),=C'---'                                                 
         MVC   P+09(08),=C'--------'                                            
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'DIV'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKDIV090 DS    0H                                                               
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         MVC   P(3),=C'DIV'                                                     
         MVC   P+5(3),KEY+4                                                     
         MVC   P+9(3),KEY+7                                                     
         B     PK2                                                              
*                                                                               
*                                                                               
*                                                                               
PKREG    DS    0H                                                               
*                                                                               
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKREG010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'REG'                                                   
         BE    PKREG090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKREG010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4                                                       
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(03),=C'CLT'                                                 
         MVC   P+09(08),=C'DIVISION'                                            
         MVC   P+18(06),=C'REGION'                                              
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(03),=C'---'                                                 
         MVC   P+09(08),=C'--------'                                            
         MVC   P+18(06),=C'------'                                              
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'REG'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKREG090 DS    0H                                                               
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         MVC   P(3),=C'REG'                                                     
         MVC   P+05(3),KEY+4       CLIENT                                       
         MVC   P+09(3),KEY+7       DIVISION                                     
         MVC   P+18(3),KEY+10      REGION                                       
         B     PK2                                                              
*                                                                               
*                                                                               
*                                                                               
PKDST    DS    0H                                                               
*                                                                               
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKDST010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'DST'                                                   
         BE    PKDST090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKDST010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4                                                       
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(03),=C'CLT'                                                 
         MVC   P+09(08),=C'DIVISION'                                            
         MVC   P+18(06),=C'REGION'                                              
         MVC   P+25(08),=C'DISTRICT'                                            
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(03),=C'---'                                                 
         MVC   P+09(08),=C'--------'                                            
         MVC   P+18(06),=C'------'                                              
         MVC   P+25(08),=C'--------'                                            
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'DST'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKDST090 DS    0H                                                               
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         MVC   P(3),=C'DST'                                                     
         MVC   P+05(3),KEY+4       CLIENT                                       
         MVC   P+09(3),KEY+7       DIVISION                                     
         MVC   P+18(3),KEY+10      REGION                                       
         MVC   P+25(3),KEY+13      DISTRICT                                     
         B     PK2                                                              
*                                                                               
*                                                                               
*                                                                               
PKEST    DS    0H                                                               
         MVC   P(3),=C'EST'                                                     
         B     PKEST5                                                           
*                                                                               
PKESTPG  DS    0H                                                               
         MVC   P(3),=C'PG '                                                     
         B     PKEST5                                                           
*                                                                               
PKESTGF  DS    0H                                                               
         MVC   P(3),=C'GF '                                                     
         B     PKEST5                                                           
*                                                                               
PKBIL    DS    0H                                                               
         MVC   P(3),=C'BIL'                                                     
         B     PKEST5                                                           
*                                                                               
PKBLF    MVC   P(3),=C'BLF'                                                     
         B     PKEST5                                                           
*                                                                               
PKBUC    MVC   P(3),=C'BKT'                                                     
         B     PKEST5                                                           
*                                                                               
PKBUD    MVC   P(3),=C'BUD'                                                     
*                                                                               
PKEST5   DS    0H                                                               
*                                                                               
         MVC   SVWORK(3),P         SAVE OFF RECORD TYPE                         
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKEST010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'EST'                                                   
         BE    PKEST090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKEST010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4                                                       
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(03),=C'CLT'                                                 
         MVC   P+09(03),=C'PRD'                                                 
         MVC   P+13(03),=C'EST'                                                 
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(03),=C'---'                                                 
         MVC   P+09(03),=C'---'                                                 
         MVC   P+13(03),=C'---'                                                 
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'EST'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKEST090 DS    0H                                                               
         MVC   P(3),SVWORK         RESTORE RECORD TYPE TO P                     
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         MVC   P+5(3),KEY+4                                                     
         MVC   P+9(3),KEY+7                                                     
         ZICM  R0,KEY+10,2                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+13(3),DUB+6(2)        ESTIMATE                                 
         B     PK2                                                              
*                                                                               
*                                                                               
*                                                                               
PKINVM   DS    0H                      INV MATCHING                             
*                                                                               
         MVC   SVPLINE,PSECOND     IF THERE'S A 2ND P LINE                      
         MVC   PSECOND,SPACES      TURN OFF 2ND P LINE FOR NOW                  
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   PKMAT010            NEW PAGE, NEED TO REPEAT HEADER              
         CLC   PKFLAG,=C'MAT'                                                   
         BE    PKMAT090                                                         
         MVC   P,SPACES                                                         
         BAS   RE,RPRT             SKIP A LINE                                  
         B     *+8                                                              
PKMAT010 MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,4                                                       
         MVC   P+00(03),=C'REC'                                                 
         MVC   P+05(03),=C'CLT'                                                 
         MVC   P+09(03),=C'PRD'                                                 
         MVC   P+14(03),=C'PUB'                                                 
         MVC   P+35(14),=C'INSERTION YEAR'                                      
         BAS   RE,RPRT                                                          
         MVC   P+00(03),=C'---'                                                 
         MVC   P+05(03),=C'---'                                                 
         MVC   P+09(03),=C'---'                                                 
         MVC   P+14(03),=C'---'                                                 
         MVC   P+35(14),=C'--------------'                                      
         BAS   RE,RPRT                                                          
         MVC   PKFLAG,=C'MAT'      SET FLAG TO CURRENT REC TYPE                 
*                                                                               
PKMAT090 DS    0H                                                               
         MVC   PSECOND,SVPLINE     RESTORING 2ND P LINE                         
*                                                                               
         MVC   WORK(25),KEY                                                     
         MVC   P(3),=C'MAT'                                                     
         MVC   P+5(3),WORK+4                                                    
         MVC   P+9(3),WORK+7                                                    
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),WORK+10),P+14                                 
*                                                                               
         MVC   P+35(1),WORK+16               YEAR                               
*                                                                               
         B     PK2                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
RPRT     NTR1                                                                   
*                                                                               
         MVC   HEAD4,SPACES                                                     
         MVC   HEAD4+47(35),=C'* TEST RUN - FILES ARE NOT MARKED *'             
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+16                                                             
         MVC   HEAD4,SPACES                                                     
         MVC   HEAD4+49(31),=C'* LIVE RUN - FILES ARE MARKED *'                 
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
RPRTX    XIT1                                                                   
*                                                                               
*                                                                               
*                                                                               
PRTXIT   DS    0H                                                               
         XMOD1                                                                  
*                                                                               
         LTORG                                                                  
         DROP  R7                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
*        PRINTING RESULTS                                                       
*                                                                               
***********************************************************************         
*                                                                               
RESULTS  CSECT                                                                  
         NMOD1 0,RESULTS                                                        
*                                                                               
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
*                                                                               
         LA    R7,1(RB)                                                         
         LA    R7,4095(R7)                                                      
         USING RESULTS+4096,R7     SECOND BASE REGISTER                         
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         IC    R1,RCSUBPRG                                                      
         LA    R1,1(R1)                                                         
         STC   R1,RCSUBPRG                                                      
         BAS   RE,RRPRT                                                         
         L     R2,ABSTAB                                                        
         L     R3,BSPARS+8         NO.IN TABLE                                  
*                                                                               
RS2      DS    0H                                                               
*                                                                               
         MVC   P,SPACES                                                         
         CLI   QOPT1,C'P'                                                       
         BE    RSPUB                                                            
         CLI   QOPT1,C'R'                                                       
         BE    RSREP                                                            
         CLI   QOPT1,C'J'                                                       
         BE    RSJOB                                                            
         CLI   QOPT1,C'C'                                                       
         BE    RSCOM                                                            
         CLI   QOPT1,C'H'                                                       
         BE    RSHDR                                                            
         CLI   QOPT1,C'L'                                                       
         BE    RSLST                                                            
         CLI   QOPT1,C'S'     DELETE CLEARANCE STATUS RECORDS ONLY              
         BE    RSCLR                                                            
*                                                                               
*                                                                               
RS19     DS    0H                                                               
         CLI   QOPT4,C'Y'                                                       
         BE    RS20               SHOW PURGED/NOT PURGED EVEN IF                
*                                 OPT2 =A                                       
         CLI   QOPT2,C'A'                                                       
         BE    RS21                                                             
         CLI   QOPT2,C'G'          OR ONLY PURGEABLE                            
         BE    RS21                                                             
*                                                                               
RS20     DS    0H                                                               
*                                                                               
         BAS   RE,RRPRT                                                         
*                                                                               
RS21     DS    0H                                                               
         A     R2,BSPARS+12                                                     
         BCT   R3,RS2                                                           
*                                                                               
         MVI   SPACING,2                                                        
         MVC   P,SPACES                                                         
         BAS   RE,RRPRT                                                         
         EDIT  (P5,PRGCNT),(9,P+20),COMMAS=YES                                  
         CLC   P+20(9),SPACES                                                   
         BH    *+8                                                              
         MVI   P+28,C'0'                                                        
         MVC   P(14),=C'RECORDS PURGED'                                         
         BAS   RE,RRPRT                                                         
         EDIT  (P5,NPRGCNT),(9,P+20),COMMAS=YES                                 
         CLC   P+20(9),SPACES                                                   
         BH    *+8                                                              
         MVI   P+28,C'0'                                                        
         MVC   P(18),=C'RECORDS NOT PURGED'                                     
         BAS   RE,RRPRT                                                         
*                                                                               
         B     RXIT                                                             
*                                                                               
*                                                                               
*                                                                               
RSPUB    DS    0H                                                               
*                                                                               
         MVC   FULL(3),=C'PUB'     ONLY 3 BYTES ARE USED                        
         BAS   RE,RESHDR           RESHDR USES FULL AND RSFLAG                  
*                                                                               
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),0(R2)),P                                      
*                                                                               
         MVC   P+18(6),=C'PURGED'                                               
         CLI   7(R2),0                                                          
         BE    RSPUB4                                                           
         AP    NPRGCNT,=P'1'                                                    
         MVC   P+18(10),=C'NOT PURGED'                                          
         B     RS19                                                             
*                                                                               
RSPUB4   DS    0H                                                               
         CLI   QOPT3,C'P'                                                       
         BNE   RSPUB5                                                           
*                                                                               
         MVC   CARD,SPACES                                                      
         MVC   CARD(05),=C'11ZZO'                                               
         MVC   CARD+15(17),P                                                    
         GOTO1 CARDS,DMCB,CARD,=C'PE00'                                         
*                                                                               
RSPUB5   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         MVC   KEY+1(6),0(R2)                                                   
         MVC   KEY+7(2),PAGYKAGY                                                
         MVI   KEY+9,X'81'                                                      
         GOTO1 HIGHPUB                                                          
         CLC   KEY(10),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'          MUST FIND PUB                                      
         GOTO1 GETNAME                                                          
         GOTO1 PUBOUT,DMCB,PUBREC,0,(X'02',P+30),0,(C'0',PUBEDIT)               
*                                                                               
         CLC   QCLIENT,=C'ALL'                                                  
         BE    RSPUB20                                                          
         CLI   QCLIENT,C' '                                                     
         BE    RSPUB20                                                          
*                          IF QCLIENT GIVEN                                     
*                          DESIGNATE WITH "**  CLT **"                          
*                                                                               
         ST    R2,FULL     SAVE TABLE'S R2                                      
         LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'14'                                                     
RSPUB10  BAS   RE,RNEXTEL                                                       
         BNE   RSPUB10X                                                         
         CLC   2(3,R2),QCLIENT                                                  
         BNE   RSPUB10                                                          
         MVC   P+85(9),=C'**     **'                                            
         MVC   P+88(3),QCLIENT                                                  
*                                                                               
RSPUB10X L     R2,FULL      RESTORE R2                                          
*                                                                               
RSPUB20  XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         MVC   KEY+1(6),0(R2)                                                   
         MVC   KEY+7(2),PAGYKAGY                                                
         MVI   KEY+9,X'81'                                                      
         GOTO1 READPUB                                                          
         OI    KEY+25,X'80'                                                     
         AP    PRGCNT,=P'1'                                                     
         GOTO1 WRTPUB                                                           
*                                                                               
         CLI   6(R2),C'L'                                                       
         BNE   RSPUB30                                                          
         MVC   P+24(2),=C'-2'                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         MVC   KEY+1(6),0(R2)                                                   
         MVC   KEY+7(2),PAGYKAGY                                                
         MVI   KEY+9,X'85'                                                      
         GOTO1 HIGHPUB                                                          
         CLC   KEY(10),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'        MUST FIND LTLREC                                     
         OI    KEY+25,X'80'                                                     
         GOTO1 WRTPUB                                                           
*                                                                               
RSPUB30  DS    0H                                                               
         BAS   RE,RRPRT                                                         
*                                                                               
*                             CHECK FOR PUB ADDRESS RECORDS                     
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         MVC   KEY+1(6),0(R2)                                                   
         MVC   KEY+7(2),PAGYKAGY                                                
         MVI   KEY+9,X'82'         PUB ADDRESS RECORDS                          
         GOTO1 HIGHPUB                                                          
         B     RSPUB50                                                          
RSPUB40  GOTO1 SEQPUB                                                           
RSPUB50  CLC   KEY(10),KEYSAVE     CHECK THRU RECORD CODE                       
         BNE   RSPUB80             LOOK FOR PUB AD SIZING RECORDS               
         TM    KEY+25,X'80'        DELETED ?                                    
         BO    RSPUB40             YES - NEXT RECORD                            
         OI    KEY+25,X'80'                                                     
         GOTO1 WRTPUB                                                           
         MVC   P+30(3),=C'PAY'                                                  
         CLI   KEY+10,X'08'        PAYING ADDRESS ?                             
         BE    RSPUB60             YES                                          
         MVC   P+30(3),=C'TRA'                                                  
         CLI   KEY+10,X'09'        TRAFFIC ADDRESS ?                            
         BE    RSPUB60             YES                                          
         MVC   P+30(3),=C'CON'                                                  
         CLI   KEY+10,X'0A'        CONTRACT ADDRESS ?                           
         BE    RSPUB60             YES                                          
         MVC   P+30(3),=C'SHI'                                                  
         CLI   KEY+10,X'0B'        SHIPPING ADDRESS ?                           
         BE    RSPUB60             YES                                          
         MVC   P+30(3),=C'???'                                                  
RSPUB60  MVC   P+34(3),KEY+11      AGENCY/CLIENT/OFFICE                         
         CLI   KEY+11,X'FF'                                                     
         BL    RSPUB70             MUST BE CLIENT                               
         MVI   P+34,C'*'                                                        
         CLI   KEY+12,X'FF'                                                     
         BL    RSPUB70             MUST BE OFFICE                               
         MVC   P+34(3),=C'ALL'     MUST BE AGENCY                               
RSPUB70  BAS   RE,RRPRT                                                         
         B     RSPUB40             NEXT RECORD                                  
         SPACE 3                                                                
*                                                                               
RSPUB80  DS    0H             CHECK FOR PUB AD SIZING RECORDS                   
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         MVC   KEY+1(6),0(R2)                                                   
         MVC   KEY+7(2),PAGYKAGY                                                
         MVI   KEY+9,X'83'         PUB AD SIZING RECORDS                        
         GOTO1 HIGHPUB                                                          
*                                                                               
         CLC   KEY(10),KEYSAVE     CHECK THRU RECORD CODE                       
         BNE   RS21                DONE - DO "RESULTS"                          
         TM    KEY+25,X'80'        DELETED ?                                    
         BO    RS21                YES - DONE - DO "RESULTS"                    
         OI    KEY+25,X'80'                                                     
         GOTO1 WRTPUB                                                           
         MVC   P+30(9),=C'AD SIZING'                                            
         BAS   RE,RRPRT                                                         
         B     RS21                DONE - DO "RESULTS"                          
*                                                                               
*                                                                               
*                                                                               
RSREP    DS    0H                                                               
*                                                                               
         MVC   FULL(3),=C'REP'     ONLY 3 BYTES ARE USED                        
         BAS   RE,RESHDR           RESHDR USES FULL AND RSFLAG                  
*                                                                               
         MVC   P(4),0(R2)                                                       
         CLI   4(R2),0                                                          
         BE    RSREP2                                                           
         MVI   P+4,C'.'                                                         
         MVC   P+5(1),4(R2)                                                     
RSREP2   MVC   P+18(6),=C'PURGED'                                               
         CLI   5(R2),0                                                          
         BE    RSREP4                                                           
         AP    NPRGCNT,=P'1'                                                    
         MVC   P+18(10),=C'NOT PURGED'                                          
         B     RS19                                                             
RSREP4   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'11'                                                      
         MVC   KEY+4(5),0(R2)                                                   
         GOTO1 READ                                                             
         OI    KEY+25,X'80'                                                     
         AP    PRGCNT,=P'1'                                                     
         GOTO1 WRT                                                              
         B     RS20                                                             
*                                                                               
*                                                                               
*                                                                               
RSJOB    DS    0H                                                               
*                                                                               
         MVC   FULL(3),=C'JOB'     ONLY 3 BYTES ARE USED                        
         BAS   RE,RESHDR           RESHDR USES FULL AND RSFLAG                  
*                                                                               
         MVC   P(3),0(R2)                                                       
         MVC   P+4(3),3(R2)                                                     
         MVC   P+8(6),6(R2)                                                     
         MVC   P+18(6),=C'PURGED'                                               
         CLI   12(R2),0                                                         
         BE    RSJOB2                                                           
         AP    NPRGCNT,=P'1'                                                    
         MVC   P+18(10),=C'NOT PURGED'                                          
         B     RS19                                                             
*                                                                               
RSJOB2   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'15'                                                      
         MVC   KEY+4(12),0(R2)                                                  
         GOTO1 HIGH                                                             
         B     RSJOB4B                                                          
RSJOB4   DS    0H                                                               
         GOTO1 SEQ                                                              
RSJOB4B  DS    0H                                                               
         CLC   KEY(16),KEYSAVE                                                  
         BNE   RS20                                                             
         GOTO1 READ                                                             
         OI    KEY+25,X'80'                                                     
         OC    KEY+16(6),KEY+16    SEE IF INSRTUCTION RECORD                    
         BNZ   RSJOB4D             DON'T ADD TO PURGE COUNT                     
         AP    PRGCNT,=P'1'                                                     
RSJOB4D  GOTO1 WRT                                                              
         B     RSJOB4                                                           
*                                                                               
*                                                                               
*                             COMMENTS                                          
RSCOM    DS    0H                                                               
*                                                                               
         MVC   FULL(3),=C'COM'     ONLY 3 BYTES ARE USED                        
         BAS   RE,RESHDR           RESHDR USES FULL AND RSFLAG                  
*                                                                               
         MVC   P(6),0(R2)                                                       
         MVC   P+18(6),=C'PURGED'                                               
         CLI   6(R2),0                                                          
         BE    RSCOM2                                                           
         AP    NPRGCNT,=P'1'                                                    
         MVC   P+18(10),=C'NOT PURGED'                                          
         B     RS19                                                             
*                                                                               
RSCOM2   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'40'                                                      
         MVC   KEY+4(6),0(R2)                                                   
         GOTO1 READ                                                             
         OI    KEY+25,X'80'                                                     
         AP    PRGCNT,=P'1'                                                     
         GOTO1 WRT                                                              
         B     RS20                                                             
*                                                                               
*                                                                               
*                                                                               
RSHDR    DS    0H                                                               
*                                                                               
         MVC   FULL(3),=C'HDR'     ONLY 3 BYTES ARE USED                        
         BAS   RE,RESHDR           RESHDR USES FULL AND RSFLAG                  
*                                                                               
         MVC   P(3),0(R2)                                                       
         MVC   P+4(3),3(R2)                                                     
         MVC   P+18(6),=C'PURGED'                                               
         CLC   6(2,R2),=X'0000'                                                 
         BE    RSHDR2                                                           
         AP    NPRGCNT,=P'1'                                                    
         MVC   P+18(10),=C'NOT PURGED'                                          
         B     RS19                                                             
*                                                                               
RSHDR2   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'02'          CLIENT HEADERS                              
         MVC   KEY+4(6),0(R2)                                                   
         GOTO1 READ                                                             
         OI    KEY+25,X'80'                                                     
         AP    PRGCNT,=P'1'                                                     
         GOTO1 WRT                                                              
         B     RS20                                                             
*                                                                               
*                                                                               
*                                                                               
RSCLR    DS    0H                                                               
*                                                                               
         MVC   FULL(3),=C'CLR'     ONLY 3 BYTES ARE USED                        
         BAS   RE,RESHDR           RESHDR USES FULL AND RSFLAG                  
*                                                                               
         MVC   P(3),0(R2)                                                       
         MVC   P+18(31),=C'CLEARANCE STATUS RECORDS PURGED'                     
         B     RS20                                                             
*                                                                               
*                                                                               
*                                                                               
RSLST    DS    0H                                                               
*                                                                               
         MVC   FULL(3),=C'LST'     ONLY 3 BYTES ARE USED                        
         BAS   RE,RESHDR           RESHDR USES FULL AND RSFLAG                  
*                                                                               
         MVC   P(3),0(R2)                                                       
         MVC   P+4(3),3(R2)                                                     
         MVC   P+18(6),=C'PURGED'                                               
         CLI   6(R2),0                                                          
         BE    RSLST2                                                           
         AP    NPRGCNT,=P'1'                                                    
         MVC   P+18(10),=C'NOT PURGED'                                          
         B     RS19                                                             
*                                                                               
RSLST2   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'17'                                                      
         MVC   KEY+4(6),0(R2)                                                   
         GOTO1 HIGH                                                             
         B     RSLST4B                                                          
RSLST4   DS    0H                                                               
         GOTO1 SEQ                                                              
RSLST4B  DS    0H                                                               
         CLC   KEY(10),KEYSAVE                                                  
         BNE   RS20                                                             
         GOTO1 READ                                                             
         OI    KEY+25,X'80'                                                     
         AP    PRGCNT,=P'1'                                                     
         GOTO1 WRT                                                              
         B     RSLST4                                                           
*                                                                               
RXIT     DS    0H                                                               
         XMOD1                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
RNEXTEL  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   RNEXTEL+2                                                        
         LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
RESHDR   NTR1                      PRINTING RESULT'S HEADER                     
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   RSHDR50             NEW PAGE, NEED TO REPEAT HEADER              
         CLC   RSFLAG,FULL                                                      
         BE    RSHDRX                                                           
         B     *+8                                                              
RSHDR50  MVI   FORCEHED,C'Y'                                                    
         MVC   P+00(05),=C'REC: '                                               
         MVC   P+05(03),FULL                                                    
         MVC   P+18(06),=C'ACTION'                                              
         BAS   RE,RRPRT                                                         
         MVC   P+00(08),=C'--------'                                            
         MVC   P+18(06),=C'------'                                              
         BAS   RE,RRPRT                                                         
         MVC   RSFLAG,FULL         SET FLAG TO CURRENT REC TYPE                 
*                                                                               
RSHDRX   B     RESXIT                                                           
*                                                                               
*                                                                               
*                                                                               
RRPRT    NTR1                                                                   
*                                                                               
         MVC   HEAD4,SPACES                                                     
         MVC   HEAD4+47(35),=C'* TEST RUN - FILES ARE NOT MARKED *'             
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+16                                                             
         MVC   HEAD4,SPACES                                                     
         MVC   HEAD4+49(31),=C'* LIVE RUN - FILES ARE MARKED *'                 
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
RESXIT   XIT1                                                                   
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
PP01WRKD DSECT                                                                  
         DS    0F                                                               
BSPARS   DS    CL24                                                             
ABSTAB   DS    A                                                                
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
PUBLSW   DS    CL1                                                              
MISSSW   DS    CL1                                                              
WRTPUBSW DS    CL1                                                              
WRTREC   DS    CL1      Y IF I NEED TO WRITE BACK CONTRACT                      
*                       WHEN PURGING COMMENTS                                   
ELHOLD   DS    CL12                                                             
SVPUBKEY DS    CL32                                                             
SVLTLKEY DS    CL32                                                             
PRGCNT   DS    PL5                                                              
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
CARD     DS    CL80                                                             
TEMPELEM DS    CL90                                                             
MYDUB    DS    D                                                                
TEMPR6   DS    F                                                                
TEMPR3   DS    F                                                                
TEMPR1   DS    F                                                                
FOUND    DS    CL1                                                              
NPRGCNT  DS    PL5                                                              
*                                                                               
SVPLINE  DS    CL132                                                            
SVWORK   DS    CL64                GENERIC EXTRA WORKING STORAGE                
PKFLAG   DS    CL3                 FLAG FOR HEADER CONTROL IN PKEYS             
RSFLAG   DS    CL3                 FLAG FOR HEADER CONTROL IN RESULTS           
*                                                                               
*                                                                               
*                                                                               
BILPROFD DSECT                                                                  
       ++INCLUDE PBILPROF                                                       
*                                                                               
CRCRECD  DSECT                                                                  
       ++INCLUDE PCRCREC                                                        
*                                                                               
***********************************************************************         
*                                                                               
BSTAB    CSECT                                                                  
         DS    400000C                                              L01         
BUFFL    EQU   400000                                               L01         
BSTAB6   EQU   400000/6                                             L01         
BSTAB8   EQU   400000/8                                             L01         
BSTAB13  EQU   400000/13                                            L01         
BSTAB7   EQU   400000/7                                             L01         
*                                                                               
USERPD   DSECT                                                                  
       ++INCLUDE PUSEREC                                                        
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
*                                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029PPREP0102S05/01/02'                                      
         END                                                                    
