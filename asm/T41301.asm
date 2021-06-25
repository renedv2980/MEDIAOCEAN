*          DATA SET T41301     AT LEVEL 131 AS OF 07/17/18                      
*PHASE T41301A                                                                  
*INCLUDE SRCHCALL                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41301- INSERTION ORDERS- HEADLINE EDIT'                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SMUR 04/30/18  SPEC-17729 NEW MEDIA CODE (D)IGITAL AUDIO                      
*                                                                               
* BPLA 07/21/15 CHANGES FOR NEW MEDIA CODES B,V,W                               
*                                                                               
* BPLA 04/13/14 CHANGES FOR MEDIA L - SOCIAL                                    
*                                                                               
* SMYE 05/26/09 EXPAND EDIT OF SECOND FAX NUMBER (AT SECFAX)                    
*                                                                               
* KWAN 03/23/06 REDUCE P72'S RETAINING TIMES BY HALF                            
*                                                                               
* BOBY 11/29/05 USE TRAFFIC OFFICE FOR ADDRESS LOOKUP IF TRAFFIC                
*        SIGNON                                                                 
*                                                                               
* SMYE 10/21/05 DEACTIVATE SPECIAL MINDSHARE PRD SECURITY IN PRD                
*                                                                               
* SMYE 06/29/05 NO-OP (SEE *SMY*) SAVE PROFILE (01/25/05 BELOW)                 
*                                                                               
* KWAN 05/31/05 CK FOR EIO SETUP RECORD                                         
*                                                                               
* SMYE 01/25/05 SAVE PROFILE FOR SHOW AD ID (NOT "ADCODE") IN SHWADID           
*                                                                               
* SMYE 07/23/04 SOX "NON-UPDATIVE" IMPLEMENTATION - SEE TRUNX                   
*                                                                               
* SMYE 02/03    ADD CALL FROM ADBUYER LOGIC                                     
*                                                                               
* KWAN 08/14/02 SECOND FAX                                                      
*                                                                               
* KWAN 05/21/02 CLIENT STRING LIMIT ACCESS                                      
*                                                                               
* KWAN 04/15/02 SAVE FROFILE FOR SHOW ADDITIONAL CHARGE IN SHWACHGR             
*                                                                               
* SMYE 04/02    SPECIAL PRODUCT SECURITY FOR CERTAIN MINDSHARE ID'S             
*                                                                               
* KWAN 08/22/01 CLIENT TRAFFIC OFFICE SECURITY                                  
*                                                                               
* KWAN 05/22/01 FIX NEW KEY SWITCH LOGIC ON ESTIMATE EQUALS ALL                 
*                                                                               
* KWAN 03/12/01 SAVE PROFILE FOR PRODUCTION HOUSE (PHOUSOPT)                    
*                                                                               
* BPLA 01/01    USE GETPROF FROM COMFACS                                        
*               INCLUDING WON'T WORK FOR PROFILES BY USERID                     
*                                                                               
* BPLA 10/00    NEW OPTION ON P72B PROFILE                                      
*               TO SUPPRESS COST NO NON-FAX COPY (WHEN FAXING)                  
*                                                                               
* BPLA 09/00    3 NEW OPTIONS ON P72B PROFILE (TAXOPT, DSPOPT, ONEEOPT)         
*               ALSO - ALWAYS REREAD ESTIMATE (NEEDED FOR NAME)                 
*                                                                               
* BPLA 06/00    NEW P72B PROFILE WITH FIRST OPTION                              
*               AIMPSOPT (DISPLAY AIMPS?)                                       
*                                                                               
* BPLA 06/00    NOW LIVE SOURCE (WAS T41301B)                                   
*               PHASE AND ++INCLUDE T413WKA RESTORED                            
*                                                                               
* BPLA 02/00    COPY OF T41301 LEVEL 104 MADE 2/8/00                            
*               T413WKAB FOR T413WKA                                            
*                                                                               
* BPLA 02/00    NEW P72A PROFILE OPTION - MULTIAD (+15)                         
*               USED IN NLPINSOR TO ALLOW MULTIPLE ADS ON AN I/O                
*                                                                               
* BPLA 01/00    SET QLMAKER TO P72 FOR FANCY LAN PRINTING                       
*                                                                               
* BPLA 10/99    NEW P72A PROFILE OPTION - RATEOPT (+14)                         
*               USED BY BLDREV FOR RATE CHG I/O                                 
*                                                                               
* BPLA 07/99    NEW P72A PROFILE OPTION (TOTOPT)                                
*               CONTROLS INSERTION ORDER $ TOTALS                               
*                                                                               
* BPLA 06/99    NEW P72A PROFILE OPTION (TLFXOPT2)                              
*               TO CONTROL THE DISPLAY OF THE TELEPHONE                         
*               AND FAX OF THE 2ND ADDRESS                                      
*                                                                               
* BPLA 02/99    ADD P72A PROFILE TO SUPPRESS DISPLAY OF                         
*               PAGE VIEWS AND CLICK THRUS                                      
*                                                                               
* BPLA 01/99    ALLOW ALL JOB/ALL PUB REQUESTS FOR ONE ESTIMATE                 
*                                                                               
* BPLA 01/99    ALLOW PUB = NNNNN,ALL  EVEN IF BASE PUB IS                      
*               ON FILE (P72 WORKS)                                             
*                                                                               
* BPLA 11/98    NEW OPTION ON THE P72A PROFILE                                  
*               +10 - SHOW WEB SITE (FROM PUBFILE ELEM)                         
*                                                                               
* BPLA 10/98    NEW OPTIONS ON THE P72A PROFILE                                 
*               +8 - SHOW DAY OF WEEK                                           
*               +9 - SHOW CLOSING DATE (SPACE)                                  
*                                                                               
* BPLA 09/98    CHANGE FOR MEDIA "I"  - NEW ENTRIES IN FORMTAB                  
*                                                                               
* SMYE 06/08/98 USE DATCON FOR CTODAY (TODAY'S DATE) AT RQ2...                  
*                                                                               
* BPLA 05/98    NEW FIELD ON P72A PROFILE (+7)                                  
*               USED TO ADJUST MAXLINES FOR SPECIAL AGENCY FORMS                
*               VALUE IS SUBTRACTED FORM MAXLINES                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T41301   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41301,RR=R2                                                   
*                                                                               
         ST    R2,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RC,0(R1)                                                         
         USING POLWRKD,RC                                                       
         USING T413FFD,RA                                                       
         USING IOWORKD,R8                                                       
         LA    R7,T41301+4095      R7 IS SECOND BASE REGISTER                   
         LA    R7,1(R7)                                                         
         USING T41301+4096,R7                                                   
*                                                                               
         LA    R6,1(R9)                                                         
         LA    R6,4095(R6)                                                      
         USING POLFILE,R9,R6                                                    
*                                                                               
         BAS   RE,INITL2                                                        
         MVI   MAXLINES,75                                                      
         MVI   RCWRITE,C'Y'                                                     
*                                                                               
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         OC    PRQMSG,PRQMSG                                                    
         BZ    RQ2                                                              
         XC    PRQMSG,PRQMSG                                                    
         FOUT  PRQMSGH                                                          
*                                                                               
RQ2      DS    0H                                                               
         MVC   ETODAY+0(2),RCDATE+6                                             
         MVC   ETODAY+2(2),RCDATE+0                                             
         MVC   ETODAY+4(2),RCDATE+3                                             
         GOTO1 DATCON,DMCB,(0,ETODAY),(3,BTODAY)                                
*                                                                               
         GOTO1 DATCON,DMCB,(0,ETODAY),(5,CTODAY)                                
*                                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         OI    DMOUTBTS,X'FD'                                                   
         MVI   DASHES,C'-'                                                      
         MVC   DASHES+1(L'DASHES-1),DASHES                                      
         MVI   STARS,C'*'                                                       
         MVC   STARS+1(L'STARS-1),STARS                                         
*                                                                               
         MVC   QRECORD,SPACES                                                   
*                                                                               
         LA    R2,PRQUIDH          VALIDATE ID                                  
         LA    R3,INVERR                                                        
         LHI   RF,D#REQID          FOR USE IN T41300 IF EDIT ERROR              
         STH   RF,SVERRFLD           OCCURS WHILE DOING ADBUYER CALL            
         CLI   5(R2),3                                                          
         BE    MED                 OK                                           
         CLI   5(R2),2                                                          
         BL    ERROR                                                            
         MVI   10(R2),C'.'         MOVE "PERIOD" TO BLANK 3RD POSITION          
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* VALIDATE MEDIA                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
MED      DS    0H                                                               
*                                                                               
* SET TO 'Y' IF MED,CLT,PRD,EST,JOB,PUB,DATES,CONTROL,TEST IS CHANGED           
*                                                                               
         MVI   NEWKSW,C'N'                                                      
*                                                                               
         LA    R2,PRQMEDH                                                       
         LA    R3,MEDERR                                                        
         LHI   RF,D#MEDCOD         FOR USE IN T41300 IF EDIT ERROR              
         STH   RF,SVERRFLD           OCCURS WHILE DOING ADBUYER CALL            
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),PRQMED                                                  
         MVI   KEY+3,1                                                          
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         MVC   AGYDA,KEY+27        SAVE DA                                      
         LA    R0,PAGYREC                                                       
         ST    R0,AREC                                                          
*                                                                               
         BAS   RE,GETPRT                                                        
*                                  SET LAST IO NUM                              
         MVC   REFNO,=H'8000'                                                   
         CLC   PAGYIODT,BTODAY                                                  
         BNE   *+10                                                             
         MVC   REFNO,PAGYIONO                                                   
*                                                                               
         MVC   PAGYIONO,REFNO                                                   
         MVC   PAGYIODT,BTODAY                                                  
*                                                                               
         LA    R4,PBUYREC                                                       
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),T413FFD+10                                           
         GOTO1 DATAMGR,DMCB,DMREAD,=CL8'CTFILE',CTIREC,CTIREC                   
*                                                                               
         TM    DMCB+8,X'FF'                                                     
         BNZ   MED10                                                            
         LA    R5,CTIDATA                                                       
*                                                                               
MED4     DS    0H                                                               
         CLI   0(R5),X'36'                                                      
         BE    MED6                                                             
         CLI   0(R5),0             EOR                                          
         BE    MED10                                                            
         SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     MED4                                                             
*                                                                               
MED6     DS    0H                                                               
         USING CTORGD,R5                                                        
         MVC   PAGYNAME,CTORGNAM                                                
         MVC   PAGYADDR,CTORGADD                                                
         DROP  R5                                                               
*                                                                               
* NOW SAVE ORIGIN ID IN AGYORIG NEEDED FOR WESTERN UNION                        
*                                                                               
MED8     LA    R4,PBUYREC                                                       
         LA    R5,CTIDATA                                                       
MED8A    CLI   0(R5),X'02'                                                      
         BE    MED8D                                                            
         CLI   0(R5),0             EOR                                          
         BE    MED10                                                            
         SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     MED8A                                                            
*                                                                               
MED8D    DS    0H                                                               
         USING CTDSCD,R5                                                        
         MVC   AGYORIG,CTDSC                                                    
         B     MED12                                                            
         DROP  R5,R4                                                            
*                                                                               
MED10    DS    0H                                                               
         DC    H'0'                                                             
MED12    DS    0H                                                               
         TM    4(R2),X'20'                                                      
         BO    CLT                                                              
         BAS   RE,CLRMED                                                        
         MVC   PRQMEDN,PAGYMED                                                  
         FOUT  PRQMEDNH                                                         
*                                                                               
         OI    4(R2),X'20'                                                      
         MVI   NEWKSW,C'Y'                                                      
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* VALIDATE CLIENT                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CLT      DS    0H                                                               
         LA    R2,PRQCLTH                                                       
         LA    R3,CLTERR                                                        
         LHI   RF,D#CLTCOD         FOR USE IN T41300 IF EDIT ERROR              
         STH   RF,SVERRFLD           OCCURS WHILE DOING ADBUYER CALL            
         OC    PRQCLT,SPACES                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,2                                                          
         MVC   KEY+4(3),PRQCLT                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         BAS   RE,GETPRT                                                        
*                                                                               
         BRAS  RE,CKSETUPR         CK FOR EIO SETUP RECORD                      
         BE    *+12                                                             
         LA    R3,EIO_YERR                                                      
         B     ERRORXT                                                          
*                                                                               
         LA    R0,PBUYREC          INIT SECBLK (TEMP USE)                       
         LHI   R1,1024                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               SAME AS XCEF, ONLY SHORTER                   
*                                                                               
         OC    4(2,RA),4(RA)       TEST AGENCY ON NEW SECURITY                  
         BNZ   *+14                                                             
         OC    6(2,RA),6(RA)       TEST ANY LIMIT ACCESS                        
         BZ    CLT2M                                                            
         L     RF,VCOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         LA    R3,PBUYREC                                                       
         GOTO1 (RF),DMCB,('SECPINIT',(R3)),0                                    
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
*                                                                               
CLT2M    MVC   BYTE2,PCLTOFF       SAVE ORIGINAL CLT OFF CODE                   
         LA    R3,CACCERR                                                       
         LA    R4,T413FFD+6                                                     
         OC    0(2,R4),0(R4)                                                    
         BZ    CLT4                NO RESTRICTIONS                              
*                                                                               
         BRAS  RE,CKTRAFID         TRAFFIC ID SIGN-ON?                          
         BNE   CLT3                NO                                           
         BRAS  RE,TRAFFACC         LOOK FOR CLIENT TRAFFIC OFFICE CODE          
         CLI   BYTE3,0             ANYTHING FOUND?                              
         BE    CLT3                NO                                           
         MVC   PCLTOFF,BYTE3       USE CLIENT TRAFFIC OFFICE CODE               
*                                                                               
CLT3     XC    WORK,WORK           WORK MUST BE AT LEAST 48 BYTES               
         LA    R1,WORK             (LENGTH OF OFFICED IS 48 BYTES)              
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAUTH,6(RA)                                                    
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,PCLTOFF                                                   
         MVC   OFCCLT,PCLTKCLT                                                  
         OC    OFCCLT,=3C' '                                                    
         MVC   OFCPMED,PCLTKMED                                                 
         MVC   OFCLMT(4),6(RA)                                                  
         LA    RE,PBUYREC                                                       
         STCM  RE,15,OFCSECD       ADDRESS OF SECRET BLOCK                      
         DROP  R1                                                               
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A38'                                           
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),255           ADDRESS OF OFFICER FOUND?                    
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORK),VCOMFACS                                   
         CLI   0(R1),0                                                          
         BNE   ERROR                                                            
*                                                                               
CLT4     DS    0H                                                               
*****    MVC   PCLTOFF,BYTE2       RESTORE CLT OFF CODE                         
         TM    4(R2),X'20'         AT THIS POINT, SECURITY IS CLEARED           
         BO    PRD                                                              
         BAS   RE,CLRCLT                                                        
         MVC   PRQCLTN,PCLTNAME                                                 
         FOUT  PRQCLTNH                                                         
*                                                                               
         XC    WORK,WORK           GET PROFILE                                  
         MVC   WORK(4),=C'P072'                                                 
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),PCLTKMED                                               
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
         L     RF,VCOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETPROF,DMCB,WORK,PROGPROF,VDATAMGR                             
*                                                                               
         DROP  RF                                                               
*                                  SET REPORT OPTIONS                           
         MVI   FORMOPT,C'*'                                                     
*                                                                               
         MVI   RPTOPTS,C'N'                                                     
         MVC   RPTOPTS+1(L'RPTOPTS-1),RPTOPTS                                   
*                                                                               
         OC    PROGPROF,PROGPROF                                                
         BZ    FR10                                                             
*                                  SET FOR PROGPORF                             
         LA    RF,PROGPROF                                                      
         MVC   MULTOPT,0(RF)                                                    
         MVC   NAMOPT(3),1(RF)                                                  
         MVC   ESTOPT(3),4(RF)                                                  
         MVC   OLDOPT,7(RF)                                                     
         MVC   MATOPT,8(RF)                                                     
         MVC   ADOPT,9(RF)                                                      
         MVC   ADDROPT,10(RF)                                                   
         MVC   COPYOPT,11(RF)     'Y' = SUPPRESS COPY                           
         MVC   MATEOPT,12(RF)     'Y' = PRINT MATERIALS CLOSING  L01            
         MVC   TLFXOPT,13(RF)      T,F,OR B=TEL,FAX OR BOTH      L02            
         MVC   REVOPT,14(RF)      'Y' = SUPPRESS REASON FOR REVISION            
         MVC   FORMOPT,15(RF)        FORMS OPTION                               
*                                       MESSAGES                                
         B     FR12                                                             
*                                                                               
FR10     DS    0H                                                               
         CLI   PAGYPROF+29,C'1'                                                 
         BNH   *+8                                                              
         MVI   MULTOPT,C'Y'        MULTI INS - PROF=2                           
         BNL   *+8                                                              
         MVI   OLDOPT,C'Y'         OLD HEADS - PROF=0                           
*                                                                               
         TM    PAGYPROF+20,X'02'   7,6,3,2, - NO NAME                           
         BZ    *+8                                                              
         MVI   NAMOPT,C'Y'                                                      
         TM    PAGYPROF+20,X'04'   7,6,5,4 - XX'S                               
         BZ    *+8                                                              
         MVI   XXOPT,C'Y'                                                       
         TM    PAGYPROF+20,X'01'   7,5,3,1 = * OUT MEMBER MSG                   
         BZ    *+8                                                              
         MVI   MEMBOPT,C'Y'                                                     
*                                                                               
         TM    PCLTPROF+16,X'04'   7,6,5,4, = COST                              
         BZ    *+8                                                              
         MVI   COSTOPT,C'Y'                                                     
         TM    PCLTPROF+16,X'01'   7,5,3,1 = VEN NO.                            
         BZ    *+8                                                              
         MVI   VNOOPT,C'Y'                                                      
         TM    PCLTPROF+16,X'02'   7,6,3,2 = EST NO.                            
         BZ    *+8                                                              
         MVI   ESTOPT,C'Y'                                                      
*                                                                               
FR12     DS    0H                  GET P72A PROFILE                             
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'P72A'                                                 
         NI    WORK,X'BF'          MAKE SYSTEM LOWER CASE                       
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),PCLTKMED                                               
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
         L     RF,VCOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETPROF,DMCB,WORK,PROGPROF,VDATAMGR                             
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVI   MAXLADJ,0           SET DEFAULT TO 0                             
*                                                                               
         OC    PROGPROF,PROGPROF                                                
         BZ    FR14                                                             
*                                                                               
         MVC   ACOPT,PROGPROF+0                                                 
         MVC   CDOPT,PROGPROF+1                                                 
         MVC   CONOPT,PROGPROF+2                                                
         MVC   MKZOPT,PROGPROF+4                                                
         MVC   ONSOPT,PROGPROF+5                                                
         MVC   FROPT,PROGPROF+6                                                 
         MVC   MAXLADJ,PROGPROF+7  MAXLINES ADJUSTMENT                          
*                                                                               
         MVC   DOWOPT,PROGPROF+8                                                
         MVC   CLOOPT,PROGPROF+9                                                
         MVC   WEBOPT,PROGPROF+10                                               
         MVC   PVCTOPT,PROGPROF+11 OPTION TO SUPPRESS PV,CT,IMPS                
*                                                                               
* TEL AND FAX FOR 2ND ADDRESS                                                   
*                                                                               
         MVC   TLFXOPT2,PROGPROF+12                                             
*                                                                               
* I/O $ TOTALS: N=NO TOTALS, G=GROSS ONLY, T=NET ONLY, B=BOTH G&N               
*                                                                               
         MVC   TOTOPT,PROGPROF+13                                               
*                                                                               
         CLI   TOTOPT,C' '                                                      
         BH    *+8                                                              
         MVI   TOTOPT,C'N'         MUST SET DEFAULT (N) IF NOT PRESENT          
*                                                                               
         MVC   RATEOPT,PROGPROF+14 RATE CHG I/O                                 
         MVC   MULTIAD,PROGPROF+15 MULTIPLE AD OPTION                           
         CLI   MULTIAD,C' '                                                     
         BH    *+8                                                              
         MVI   MULTIAD,C'N'        MUST SET DEFAULT (N) IF NOT PRESENT          
*                                                                               
FR14     DS    0H                  GET P72A PROFILE                             
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'P72B'                                                 
         NI    WORK,X'BF'          MAKE SYSTEM LOWER CASE                       
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),PCLTKMED                                               
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
         L     RF,VCOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETPROF,DMCB,WORK,PROGPROF,VDATAMGR                             
         DROP  RF                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         MVC   AIMPSOPT,PROGPROF+0 DISPLAY AIMPS?                               
         CLI   AIMPSOPT,C' '                                                    
         BH    *+8                                                              
         MVI   AIMPSOPT,C'N'       MUST SET DEFAULT (N) IF NOT PRESENT          
*                                                                               
         MVC   TAXOPT,PROGPROF+1   TAX OPTION                                   
         CLI   TAXOPT,C' '                                                      
         BH    *+8                                                              
         MVI   TAXOPT,C'N'         MUST SET DEFAULT (N) IF NOT PRESENT          
*                                                                               
         MVC   DSPOPT,PROGPROF+2   DOUBLE SPACE INS. INFO                       
         CLI   DSPOPT,C' '                                                      
         BH    *+8                                                              
         MVI   DSPOPT,C'N'         MUST SET DEFAULT (N) IF NOT PRESENT          
*                                                                               
         MVC   ONEEOPT,PROGPROF+3  ONE EST. REQ - EST IN HEADS                  
         CLI   ONEEOPT,C' '                                                     
         BH    *+8                                                              
         MVI   ONEEOPT,C'N'        MUST SET DEFAULT (N) IF NOT PRESENT          
*                                                                               
         MVC   NCOPT,PROGPROF+4    SUPPRESS COST ON NON-FAX COPY                
         CLI   NCOPT,C' '          WHEN FAXING                                  
         BH    *+8                                                              
         MVI   NCOPT,C'N'          MUST SET DEFAULT (N) IF NOT PRESENT          
*                                                                               
         MVC   PHOUSOPT,PROGPROF+5 SHOW PRODUCTION HOUSE                        
         CLI   PHOUSOPT,C' '                                                    
         BH    *+8                                                              
         MVI   PHOUSOPT,C'N'       MUST SET DEFAULT (N) IF NOT PRESENT          
*                                                                               
         MVC   SHWACHGR,PROGPROF+6 SHOW ADDITIONAL CHARGES                      
         CLI   SHWACHGR,C' '                                                    
         BH    *+8                                                              
         MVI   SHWACHGR,C'N'       MUST SET DEFAULT (N) IF NOT PRESENT          
*                                                                               
*SMY*    MVC   SHWADID,PROGPROF+7  SHOW AD ID IN PLACE OF ADCODE                
*SMY*    CLI   SHWADID,C' '                                                     
*SMY*    BH    *+8                                                              
*SMY*    MVI   SHWADID,C'N'        MUST SET DEFAULT (N) IF NOT PRESENT          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FR15     DS    0H                  GET PW PROFILE                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'P0PW'                                                 
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),PCLTKMED                                               
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
         L     RF,VCOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETPROF,DMCB,WORK,PROGPROF,VDATAMGR                             
         DROP  RF                                                               
*                                                                               
         OI    4(R2),X'20'         SET REPORT OPTIONS                           
         MVI   NEWKSW,C'Y'                                                      
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRD      DS    0H                  VALIDATE PRQDUCT                             
         LA    R2,PRQPRDH                                                       
         LA    R3,PRDERR                                                        
         LHI   RF,D#PRDCOD         FOR USE IN T41300 IF EDIT ERROR              
         STH   RF,SVERRFLD           OCCURS WHILE DOING ADBUYER CALL            
         CLI   PRQPRD,C'*'         DON'T ALLOW OTHER AGY PRD                    
         BE    ERROR                                                            
         OC    PRQPRD,SPACES                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(7),PCLTKEY                                                   
         MVI   KEY+3,6                                                          
         MVC   KEY+7(3),PRQPRD                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         LA    R0,PPRDREC                                                       
         ST    R0,AREC                                                          
         BAS   RE,GETPRT                                                        
*                                                                               
         B     PRDOK               BRANCH AROUND DISCONTINUED LOGIC             
*                                                                               
*   10/21/05 - BELOW LOGIC DISCONTINUED AT AGENCY REQUEST                       
*                                                                               
         LR    RE,RA               SPECIAL MINDSHARE PRD SECUTITY               
         USING TWAD,RE                                                          
         ZICM  RF,TWAUSRID,2                                                    
         DROP  RE                                                               
         CHI   RF,10296            "MSSROC" USER ID ?                           
         BE    PRDCKB              YES                                          
         CHI   RF,10297            "MSSRYC" USER ID ?                           
         BE    PRDCKC              YES                                          
         B     PRDOK                                                            
*                                                                               
PRDCKB   DS    0H                                                               
         CLI   PPRDTRAF,C'6'       TRAFFIC OFFICE = 6 ?                         
         BE    PRDOK               YES - OK                                     
         B     PRDCKNG             SECURITY LOCKOUT                             
*                                                                               
PRDCKC   DS    0H                                                               
         CLI   PPRDTRAF,C'7'       TRAFFIC OFFICE = 7 ?                         
         BE    PRDOK               YES - OK                                     
PRDCKNG  LA    R3,PPESECLK         SECURITY LOCKOUT                             
         B     ERROR                                                            
*                                                                               
*    10/21/05 - ABOVE LOGIC DISCONTINUED AT AGENCY REQUEST                      
*                                                                               
PRDOK    DS    0H                                                               
         TM    4(R2),X'20'                                                      
         BO    EST                                                              
         BAS   RE,CLRPRD                                                        
         MVC   PRQPRDN,PPRDNAME                                                 
         FOUT  PRQPRDNH                                                         
*                                                                               
         OI    4(R2),X'20'                                                      
         MVI   NEWKSW,C'Y'                                                      
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EST      DS    0H                  VALIDATE EST                                 
         LA    R2,PRQESTH                                                       
         LA    R3,ESTERR                                                        
         LHI   RF,D#ESTNUM         FOR USE IN T41300 IF EDIT ERROR              
         STH   RF,SVERRFLD           OCCURS WHILE DOING ADBUYER CALL            
         XC    PRQESTN,PRQESTN                                                  
         FOUT  PRQESTNH                                                         
*                                                                               
         OC    PRQEST,SPACES                                                    
         CLC   PRQEST,SPACES                                                    
         BNE   EST2                                                             
         MVC   PRQEST,=C'ALL'                                                   
         FOUT  PRQESTH                                                          
*                                                                               
EST2     DS    0H                                                               
         CLC   PRQEST,=C'ALL'                                                   
         BE    EST3                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(10),PPRDKEY                                                  
         MVI   KEY+3,7                                                          
         BAS   RE,PACK                                                          
         STH   R0,KEY+10                                                        
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         LA    R0,PESTREC                                                       
         ST    R0,AREC                                                          
         BAS   RE,GETPRT                                                        
         TM    PESTTEST,X'80'          SEE IF TEST ESTIMATE                     
         BZ    EST3H                                                            
         MVC   PRQESTN(20),=C'*THIS IS A TEST EST*'                             
         FOUT  PRQESTNH                                                         
         LA    R3,INVERR               NO INS ORDERS FOR TEST ESTS              
         B     ERROR                                                            
*                                                                               
EST3     DS    0H                                                               
         XC    PRQESTN,PRQESTN                                                  
         MVC   PRQESTN(13),=C'ALL ESTIMATES'                                    
         OI    6(R2),X'80'                                                      
         TM    4(R2),X'20'                                                      
         BO    JOB                                                              
         B     EST4                                                             
*                                                                               
* ONEEOPT - CHECK AFTER ESTIMATE IS READ                                        
*                                                                               
EST3H    TM    4(R2),X'20'                                                      
         BO    JOB                                                              
*                                                                               
* ONEEOPT - END                                                                 
*                                                                               
         UNPK  PRQEST,DUB                                                       
         FOUT  PRQESTH                                                          
         MVC   PRQESTN,PESTNAME                                                 
         FOUT  PRQESTNH                                                         
*                                                                               
EST4     DS    0H                                                               
         OI    4(R2),X'20'                                                      
         MVI   NEWKSW,C'Y'                                                      
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* VALIDATE JOB                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
JOB      DS    0H                                                               
         LA    R2,PRQJOBH                                                       
         LA    R3,JOBERR                                                        
         LHI   RF,D#ADCODE         FOR USE IN T41300 IF EDIT ERROR              
         STH   RF,SVERRFLD           OCCURS WHILE DOING ADBUYER CALL            
         OC    PRQJOB,SPACES                                                    
         TM    4(R2),X'20'                                                      
         BO    PUB                                                              
         CLC   PRQJOB,SPACES                                                    
         BNE   JOB2                                                             
         MVC   PRQJOB(3),=C'ALL'                                                
         FOUT  PRQJOBH                                                          
JOB2     DS    0H                                                               
         CLC   PRQJOB,=CL6'ALL'                                                 
         BE    JOB4                                                             
*                                                                               
         CLI   PRQJOB,C'!'         BEGINNING ! (GROUP CODE) ?                   
         BNE   JOB3                NO                                           
         LA    R3,NOGRPJOB         GROUP CODES NOT ALLOWED                      
         B     ERRORXT                                                          
*                                                                               
JOB3     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(10),PPRDKEY                                                  
         MVI   KEY+3,X'15'                                                      
         MVC   KEY+10(6),PRQJOB                                                 
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
*                                                                               
JOB4     DS    0H                                                               
         OI    4(R2),X'20'                                                      
         MVI   NEWKSW,C'Y'                                                      
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* VALIDATE PUBLICATION                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PUB      DS    0H                                                               
         XC    MYFAX,MYFAX                                                      
*                                                                               
         MVI   ZEOPT,C'N'          MULTI-E/Z OPT                                
         LA    R2,PRQPUBH                                                       
*                                                                               
* NAME SEARCH CALL                                                              
*                                                                               
         SR    R2,RA               GET DISPLACEMENT INTO TWA OF PUB             
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         XC    DSPARM(DSPARML),DSPARM                                           
         MVC   DSMEDCOD,PRQMED                                                  
*                                                                               
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',(RA)),VCOMFACS,       +        
               ('DSPARML',WORK),(1,=CL8'PUB'),0,RR=RELO                         
         DROP  R3                                                               
         LA    R2,PRQPUBH                                                       
*                                                                               
         LA    R3,PUBERR                                                        
         LHI   RF,D#PUBCOD         FOR USE IN T41300 IF EDIT ERROR              
         STH   RF,SVERRFLD           OCCURS WHILE DOING ADBUYER CALL            
         OC    PRQPUB,SPACES                                                    
         CLC   PRQPUB,SPACES                                                    
         BE    PUB1                                                             
         CLC   PRQPUB(3),=C'ALL'                                                
         BNE   PUB2                                                             
*                                                                               
         CLC   PRQEST(3),=C'ALL'   FOR ALL ESTIMATE REQUESTS                    
         BNE   PUB0                                                             
         CLC   PRQJOB,=CL6'ALL'                                                 
         BE    ERROR               DON'T ALLOW ALL JOBS AND ALL PUBS            
*                                                                               
PUB0     FOUT  PRQPUBH                                                          
         XC    PRQPUBN,PRQPUBN                                                  
         FOUT  PRQPUBNH                                                         
         B     PUB2                                                             
*                                                                               
PUB1     DS    0H                                                               
         B     ERROR               PUB MISSING                                  
*                                                                               
PUB2     DS    0H                  'ALL' PUBS OK                                
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         MVC   WORK(17),PRQPUB                                                  
         LA    RF,WORK                                                          
         AR    RF,R0                                                            
         AHI   RF,-4                                                            
         CLC   0(4,RF),=C',ALL'                                                 
         BNE   PUB3                                                             
*                                                                               
         CLC   PRQPUB(3),=C'ALL'   DON'T ALLOW 'ALL,ALL'                        
         BE    ERROR                                                            
*                                                                               
         MVC   0(4,RF),SPACES                                                   
         AHI   R0,-4                                                            
         MVI   ZEOPT,C'Y'          SET DOING MULTI EDITS/ZONES                  
*                                                                               
* ZEOPT MIGHT BE RESET IN BLDQ IF MULTOPT NOT EQ 'Y'                            
*                                                                               
         CLI   MULTOPT,C'Y'        SEE IF DOING MULTI-IO'S                      
         BNE   PUB3                                                             
*                                                                               
         MVI   MATOPT,C'Y'         SET MATERIALS ACROSS Z/E                     
*                                                                               
PUB3     DS    0H                  NB- BPUB+4(2)=NULLS                          
         XC    BPUB,BPUB                                                        
         XC    PUBREC(50),PUBREC                                                
         XC    PRQPUBN,PRQPUBN                                                  
         CLC   WORK(3),=C'ALL'                                                  
         BE    PUB3D                                                            
*                                                                               
         GOTOR APUBVAL,DMCB,((R0),WORK),DUB                                     
*                                                                               
         MVC   BPUB,DUB                                                         
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         BAS   RE,FNDPUB                                                        
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
         MVC   PRQPUBN,PUBNAME                                                  
PUB3D    FOUT  PRQPUBNH                                                         
*                                                                               
PUB4     TM    4(R2),X'20'                                                      
         BO    PER                                                              
         MVI   NEWKSW,C'Y'                                                      
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PER      DS    0H                  VALIDATE STANT-END DATES                     
         LA    R2,PRQPERH                                                       
         LA    R3,DATERR                                                        
         LHI   RF,D#STEND          FOR USE IN T41300 IF EDIT ERROR              
         STH   RF,SVERRFLD           OCCURS WHILE DOING ADBUYER CALL            
         GOTO1 DATVAL,DMCB,8(R2),QSTART                                         
*                                                                               
         CLI   DMCB+3,0                                                         
         BE    ERROR                                                            
         ZIC   R0,5(R2)                                                         
         S     R0,DMCB             SUBTRACT LENGHT OF FIRST DATE                
         CHI   R0,0                                                             
         BE    PER6                                                             
         CHI   R0,4                                                             
         BL    PER4                LINE NUMBER GIVEN                            
         L     R5,DMCB                                                          
         LA    R5,9(R2,R5)                                                      
         GOTO1 (RF),(R1),(R5),QEND                                              
*                                                                               
         CLI   DMCB+3,0                                                         
         BE    ERROR                                                            
         B     PER8                                                             
*                                  LINE NUMBER GIVEN                            
PER4     DS    0H                                                               
         L     R5,DMCB                                                          
         LA    R5,9(R2,R5)                                                      
         CLC   PRQEST(3),=C'ALL'                                                
         BE    ERROR                                                            
         MVC   QBUYLIN+1(1),0(R5)                                               
         CLI   0(R5),C'0'                                                       
         BL    ERROR               CHK FOR NUMERICS                             
         CLI   0(R5),C'9'                                                       
         BH    ERROR                                                            
         CHI   R0,2                SEE IF -N ENTERED                            
         BNE   PER5                                                             
         MVI   QBUYLIN,C'0'                                                     
         B     PER6                                                             
*                                                                               
PER5     CHI   R0,3                SEE IF -NN ENTERED                           
         BNE   ERROR                                                            
         CLI   1(R5),C'0'                                                       
         BL    ERROR               CHK FOR NUMERICS                             
         CLI   1(R5),C'9'                                                       
         BH    ERROR                                                            
         MVC   QBUYLIN,0(R5)                                                    
*                                                                               
PER6     DS    0H                                                               
         MVC   QEND,QSTART                                                      
*                                                                               
PER8     DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(3,BSTART)                                
*                                                                               
         GOTO1 DATCON,(R1),(0,QEND),(3,BEND)                                    
*                                                                               
         TM    4(R2),X'20'                                                      
         BO    CDT                                                              
         MVI   NEWKSW,C'Y'                                                      
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CDT      DS    0H                  VALIDATE CONTROL DATE                        
         LA    R2,PRQCDTH                                                       
         LA    R3,CTLERR                                                        
         LHI   RF,D#CTDTE          FOR USE IN T41300 IF EDIT ERROR              
         STH   RF,SVERRFLD           OCCURS WHILE DOING ADBUYER CALL            
         MVC   BIGNDAT,=3X'FF'                                                  
         CLI   5(R2),0                                                          
         BE    CDTX                                                             
         GOTO1 DATVAL,DMCB,8(R2),QIGNDAT                                        
*                                                                               
         CLI   DMCB+3,0                                                         
         BE    ERROR                                                            
         GOTO1 DATCON,DMCB,(0,QIGNDAT),(3,BIGNDAT)                              
*                                                                               
CDTX     DS    0H                                                               
         TM    4(R2),X'20'                                                      
         BO    TRUN                                                             
         MVI   NEWKSW,C'Y'                                                      
         OI    4(R2),X'20'                                                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TRUN     LA    R2,PRQRUNH          EDIT TEST RUN FIELD                          
         CLI   5(R2),0                                                          
         BE    TRUNX                                                            
         CLI   8(R2),C'N'                                                       
         BE    TRUNX               LIVE RUN                                     
         LA    R3,INVERR                                                        
         LHI   RF,D#TSTYN          FOR USE IN T41300 IF EDIT ERROR              
         STH   RF,SVERRFLD           OCCURS WHILE DOING ADBUYER CALL            
         CLI   8(R2),C'Y'                                                       
         BNE   TRUN5                                                            
         MVI   RCWRITE,C'N'        DON'T MARK FILE                              
         B     TRUNX3                                                           
*                                                                               
TRUN5    CLI   8(R2),C'F'          FAX RUN                                      
         BNE   ERROR                                                            
         MVI   MAXLINES,74         MUST SET TO 74 FOR FAX REQUESTS              
         CLI   PWPROF1,C'Y'        SEE IF FAXING IS ALLOWED                     
         BNE   ERROR                                                            
*                                                                               
TRUNX    DS    0H                  TEST IF USER IS "NON-UPDATIVE"               
         L     RF,VCOMFACS                                                      
         L     RF,CXTRAINF-COMFACSD(RF)                                         
         USING XTRAINFD,RF                                                      
         TM    XIFLAG1,X'E0'       READ ONLY OR WRONG FACPAK ?                  
         BZ    TRUNX3              NO - OK                                      
         LHI   RF,D#TSTYN          FOR USE IN T41300 IF EDIT ERROR              
         STH   RF,SVERRFLD           OCCURS WHILE DOING ADBUYER CALL            
         LA    R3,NOTUPD          NO UPDATES IN THIS ONLINE APPLICATION         
         B     ERRORXT                                                          
*                                                                               
TRUNX3   DS    0H                                                               
         CLI   MAXLADJ,0           CHECK FOR MAXLINES ADJUSTMENT                
         BE    TRUNX5                                                           
         ZIC   R0,MAXLINES                                                      
         ZIC   R1,MAXLADJ                                                       
         SR    R0,R1                                                            
         STC   R0,MAXLINES                                                      
*                                                                               
TRUNX5   DS    0H                                                               
         TM    4(R2),X'20'                                                      
         BO    NEED                                                             
         MVI   NEWKSW,C'Y'         SET FOR NEW KEY                              
         OI    4(R2),X'20'                                                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NEED     LA    R2,PRQNEEDH         EDIT NEEDED ONLY OPTION                      
         CLI   5(R2),0                                                          
         BE    NEEDX                                                            
         CLI   8(R2),C'N'                                                       
         BE    NEEDX                                                            
         LA    R3,INVERR                                                        
         LHI   RF,D#NDOYN          FOR USE IN T41300 IF EDIT ERROR              
         STH   RF,SVERRFLD           OCCURS WHILE DOING ADBUYER CALL            
         CLI   8(R2),C'Y'          WILL SET QOPT 2 TO "Y"                       
         BE    NEEDX                                                            
         B     ERROR                                                            
*                                                                               
NEEDX    DS    0H                                                               
         TM    4(R2),X'20'                                                      
         BO    SECFAX                                                           
         MVI   NEWKSW,C'Y'         SET FOR NEW KEY                              
         OI    4(R2),X'20'                                                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SECFAX   LA    R2,PRQFAX2H         EDIT SECOND FAX                              
         XC    SECONDFX,SECONDFX                                                
         CLI   5(R2),0                                                          
         BE    SFAXX                                                            
*                                                                               
         LA    R3,INVERR                                                        
         LHI   RF,D#FAX#2          FOR USE IN T41300 IF EDIT ERROR              
         STH   RF,SVERRFLD           OCCURS WHILE DOING ADBUYER CALL            
         CLI   PRQRUN,C'F'         FAX RUN?                                     
         BNE   ERROR               INVALID FOR 2ND FAX IF NOT FAXING            
         CLI   5(R2),12            MAX INPUT IS 12 CHARS                        
         BH    ERROR                                                            
         CLI   5(R2),4             MIN INPUT IS 4 (FX=?)                        
         BL    ERROR                                                            
*                                                                               
         MVC   SECONDFX,SPACES     FAX CODE IS SPACE PADDED (IN CASE)           
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SECONDFX(0),8(R2)                                                
*                                                                               
         CLC   =C'FX=',8(R2)       FAX NUMBER IS IN CONTROL FILE?               
         BNE   SFAXED                                                           
         BRAS  RE,CKCTFFAX         CK FAX CODE IS ON CONTROL FILE               
         BNE   ERROR                                                            
         B     SFAXX                                                            
*                                                                               
SFAXED   DS    0H                                                               
*                                                                               
*        FAX NUMBER MUST CONTAIN BETWEEN 10 AND 12 CHARACTERS                   
*                                                                               
*        EXTRACT ALL NUMERIC DIGITS FROM INPUT                                  
*                                                                               
         ZIC   R0,5(R2)            GET LENGTH OF INPUT                          
         XC    WORK,WORK           INIT WORKAREA                                
         LA    RF,8(R2)            POINT TO INPUT                               
         LA    RE,WORK                                                          
*                                                                               
SFAXLP   DS    0H                                                               
*                                                                               
         CLI   0(RF),C'0'          DROP NON-NUMERIC CHARACTERS                  
         BL    SFAXCN                                                           
         CLI   0(RF),C'9'                                                       
         BH    SFAXCN                                                           
*                                                                               
         MVC   0(1,RE),0(RF)       SAVE NUMERIC DIGIT                           
         AHI   RE,1                BUMP TO NEXT SAVE POSITION                   
*                                                                               
SFAXCN   DS    0H                                                               
*                                                                               
         AHI   RF,1                BUMP TO NEXT INPUT CHARACTER                 
         BCT   R0,SFAXLP                                                        
*                                                                               
         LA    RF,WORK             CALCULATE LENGTH OF FAX NUMBER               
         SR    RE,RF                                                            
*                                                                               
         CHI   RE,10               MUST BE EXACTLY 10 DIGITS                    
         BNE   ERROR                                                            
*                                                                               
         XC    8(12,R2),8(R2)      CLEAR SECOND FAX FIELD                       
         STC   RE,5(R2)            SET FIELD LENGTH (MUST BE 10)                
         BCTR  RE,0                PREP FOR EXECUTED MOVE                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),WORK        SAVE FAX NUMBER WITHOUT SEPARATORS           
         MVC   SECONDFX,SPACES     AND "REPLACE" SECONDFX FIELD                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SECONDFX(0),WORK                                                 
*                                                                               
SFAXX    DS    0H                                                               
         TM    4(R2),X'20'                                                      
         BO    SUPCOS                                                           
         MVI   NEWKSW,C'Y'         SET FOR NEW KEY                              
         OI    4(R2),X'20'                                                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SUPCOS   LA    R2,PRQSCOSH         EDIT SUPPRESS COST ON 2ND FAX                
         MVI   SUPCOSSF,C'N'       DO NOT SUPPRESS COST ON 2ND FAX              
         MVI   NUMACLNS,0          INIT NUMBER OF ADDTNL CHRGS LINES            
         CLI   5(R2),0                                                          
         BE    SUPCOSX                                                          
*                                                                               
         LA    R3,INVERR                                                        
         LHI   RF,D#SPCST          FOR USE IN T41300 IF EDIT ERROR              
         STH   RF,SVERRFLD           OCCURS WHILE DOING ADBUYER CALL            
         OC    SECONDFX,SECONDFX   2ND FAX NUMBER IS PRESENT?                   
         BZ    ERROR               INVALID IF 2ND FAX FLD IS EMPTY              
*                                                                               
         CLI   8(R2),C'Y'                                                       
         BNE   ERROR               ONLY ACCEPTS "Y"                             
         MVI   SUPCOSSF,C'Y'       SUPPRESS COST ON 2ND FAX                     
*                                                                               
SUPCOSX  TM    4(R2),X'20'                                                      
         BO    INSCOM                                                           
         MVI   NEWKSW,C'Y'         SET FOR NEW KEY                              
         OI    4(R2),X'20'                                                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
INSCOM   DS    0H                                                               
*                                                                               
         LA    R2,PRQCOM1H                                                      
         MVC   INSCOMM(50),PRQCOM1  SAVE COM1                                   
*                                                                               
         TM    4(R2),X'20'                                                      
         BO    INSCOM2                                                          
         MVI   NEWKSW,C'Y'         SET FOR NEW KEY                              
         OI    4(R2),X'20'                                                      
*                                                                               
INSCOM2  LA    R2,PRQCOM2H                                                      
         MVC   INSCOMM+50(50),PRQCOM2   SAVE COM2                               
         TM    4(R2),X'20'                                                      
         BO    INSCOM3                                                          
         MVI   NEWKSW,C'Y'         SET FOR NEW KEY                              
         OI    4(R2),X'20'                                                      
*                                                                               
INSCOM3  LA    R2,PRQCOM3H                                                      
         MVC   INSCOMM+100(50),PRQCOM3 SAVE COM3                                
         TM    4(R2),X'20'                                                      
         BO    BLDQ                                                             
         MVI   NEWKSW,C'Y'         SET FOR NEW KEY                              
         OI    4(R2),X'20'                                                      
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BLDQ     DS    0H                  BUILD QRECORD                                
         XC    SVERRFLD,SVERRFLD   NO EDIT ERRORS                               
         MVC   QCLIENT,PRQCLT                                                   
         MVC   QPRODUCT,PRQPRD                                                  
         MVC   QJOB,PRQJOB                                                      
         MVC   QEST,PRQEST                                                      
         MVC   QPUB(3),=C'ALL'                                                  
         CLC   PRQPUB(3),=C'ALL'                                                
         BE    BLDQ2                                                            
         GOTO1 APUBEDIT,DMCB,BPUB,(C'Q',QPUB)                                   
*                                                                               
BLDQ2    DS    0H                                                               
         CLI   ZEOPT,C'Y'          TEST MULTI ZONES/EDITS                       
         BNE   BLDQ5                                                            
         MVC   QPUB+8(3),=C'ZZZ'                                                
*                                                                               
         CLI   MULTOPT,C'Y'                                                     
         BE    BLDQ5                                                            
         MVI   ZEOPT,C'N'          ONLY LEAVE ZEOPT 'Y' IF MULTOPT=Y            
*                                                                               
BLDQ5    MVC   QPROG,=C'72'                                                     
         MVC   QAGENCY,AGYALPHA                                                 
         MVC   QMEDIA,PRQMED                                                    
         MVI   QOPT3,C'T'          REMOTE TERM                                  
*                                                                               
         CLI   PRQRUN,C'F'         SEE IF FAXING                                
         BNE   *+8                                                              
         MVI   QOPT7,C'F'                                                       
*                                                                               
         CLI   PRQNEED,C'Y'        SEE REQUESTING "NEEDED ONLY"                 
         BNE   *+8                                                              
         MVI   QOPT2,C'Y'                                                       
*                                                                               
         MVC   RCSVAGY(3),PAGYKAGY                                              
         MVC   RCSVCLI,PCLTKCLT                                                 
         MVC   RCSVPRD,PPRDKPRD                                                 
*                                                                               
         L     RF,VTIA             INITIALIZE FOR PRINTING                      
         XC    0(200,RF),0(RF)                                                  
         MVC   0(5,RF),=C'START'                                                
         LA    RF,4(RF)            1ST BYTE OF PQPLD IS SET IN OLPRINT          
*                                                                               
         USING PQPLD,RF                                                         
         MVC   QLDESC,=C'IO-XNNNN   '                                           
         MVC   QLDESC+3(1),PRQMED                                               
         LH    R0,REFNO                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QLDESC+4(4),DUB                                                  
         MVC   RCJOB,QLDESC        SAVE 'JOB'                                   
*                                                                               
         MVC   QLMAKER(3),=C'P72'  NEEDED FOR FANCY LAN PRINTING                
*                                                                               
         MVC   QLSUBID,PRQUID                                                   
         MVI   QLEXTRA,X'FF'                                                    
         MVC   QLRETNL,=H'24'      24 HOURS RETAINED UNTIL PRINTED              
         MVC   QLRETND,=H'06'      06 HOURS AFTER PRINTING                      
*                                                                               
         CLI   FORMOPT,C' '                                                     
         BNH   BLDQ5H                                                           
         CLI   FORMOPT,C'*'                                                     
         BE    BLDQ5H                                                           
         LA    R3,FORMTAB                                                       
         MVC   WORK(1),QMEDIA                                                   
         MVC   WORK+1(1),FORMOPT                                                
BLDQ5D   CLC   0(2,R3),=X'FFFF'    END OF TABLE                                 
         BE    BLDQ6               DON'T SET FORMS                              
         CLC   0(2,R3),WORK                                                     
         BE    BLDQ5T                                                           
         LA    R3,8(R3)                                                         
         B     BLDQ5D                                                           
*                                                                               
* FIRST TRY FOR ORIGINATING OFFICE OVERRIDE                                     
*                                                                               
BLDQ5H   DS    0H                                                               
         LA    R4,PBUYREC                                                       
         USING CTPREC,R4                                                        
         XC    CTPKEY,CTPKEY                                                    
         MVI   CTPKTYP,C'P'                                                     
         MVI   CTPKSYS,C'P'        PRINTPAK                                     
         MVC   CTPKPROG,=C'72'                                                  
         MVC   CTPKORIG,T413FFD+10                                              
         GOTO1 DATAMGR,DMCB,DMREAD,=CL8'CTFILE',CTPREC,CTPREC                   
*                                                                               
         TM    DMCB+8,X'FF'                                                     
         BZ    BLDQ5K                                                           
*                                                                               
         XC    CTPKEY,CTPKEY       NOW TRY FOR DEFAULT                          
         MVI   CTPKTYP,C'P'                                                     
         MVI   CTPKSYS,C'P'        PRINTPAK                                     
         MVC   CTPKPROG,=C'72'                                                  
         GOTO1 DATAMGR,DMCB,DMREAD,=CL8'CTFILE',CTPREC,CTPREC                   
*                                                                               
         TM    DMCB+8,X'FF'                                                     
         BZ    BLDQ5K                                                           
         B     BLDQ6               NOT FOUND - JUST LEAVE AS ZEROS              
*                                                                               
BLDQ5K   DS    0H                                                               
         LA    R5,CTPDATA                                                       
BLDQ5L   CLI   0(R5),X'42'                                                      
         BE    BLDQ5M                                                           
         CLI   0(R5),0                                                          
         BE    BLDQ6               OUTPUT ELEM NOT FOUND                        
         SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     BLDQ5L                                                           
*                                                                               
BLDQ5M   DS    0H                                                               
         USING CTOCOD,R5                                                        
         MVC   WORK(1),QMEDIA                                                   
         MVI   WORK+1,C'N'                                                      
         CLC   CTOCODE(4),=C'&&4AS'                                             
         BE    BLDQ5N              NEW FORMS                                    
         MVI   WORK+1,C'L'                                                      
         CLC   CTOCODE(4),=C'&&CON'                                             
         BE    BLDQ5N              OLD FORMS                                    
         B     BLDQ6               IF NOT ONE OF THOSE JUST SKIP                
*                                                                               
BLDQ5N   LA    R3,FORMTAB                                                       
         L     RF,VTIA             MUST RESET RF                                
         LA    RF,4(RF)                                                         
         B     BLDQ5D              GO GET FORM FROM TABLE                       
*                                                                               
BLDQ5T   MVC   QLFORMS(4),2(R3)                                                 
         DROP  R4,R5                                                            
*                                                                               
BLDQ6    CLI   QOPT7,C'F'          SEE IF FAXING                                
         BNE   BLDQ8                                                            
*                                                                               
         L     RF,AFAXBUF                                                       
         BRAS  RE,SETFAXBF                                                      
*                                                                               
         OC    SECONDFX,SECONDFX   SECOND FAX PRESENT?                          
         BZ    BLDQ8                                                            
         L     RF,AFAXBUF2                                                      
         BRAS  RE,SETFAXBF                                                      
*                                                                               
BLDQ8    DS    0H                                                               
         LA    RF,BUYDALST+4                                                    
         ST    RF,BUYDALST                                                      
         XC    0(4,RF),0(RF)                                                    
*                                                                               
         B     EXXMOD              SO I WON'T SET CURSOR                        
*                                                                               
FORMTAB  DC    C'MN',C'XMZN  '     MAGS - NEW                                   
         DC    C'SN',C'XMZN  '     SUPPLEMENTS - NEW                            
         DC    C'TN',C'XMZN  '     TRADE - NEW                                  
         DC    C'IN',C'XMZN  '     INTERACTIVE - NEW                            
         DC    C'LN',C'XMZN  '     SOCIAL - NEW                                 
         DC    C'NN',C'XNPR  '     NEWSPAPERS - NEW                             
         DC    C'ON',C'XODR  '     OUTDOOR - NEW                                
         DC    C'BN',C'XMZN  '     MOBILE  - NEW                                
         DC    C'VN',C'XMZN  '     NAT. VIDEO - NEW                             
         DC    C'WN',C'XMZN  '     LOC. VIDEO - NEW                             
         DC    C'DN',C'XMZN  '     DIG. AUDIO - NEW                             
*                                                                               
         DC    C'ML',C'5MAG  '     MAGS - OLD                                   
         DC    C'SL',C'5MAG  '     SUPPLEMENTS - OLD                            
         DC    C'TL',C'5MAG  '     TRADE - OLD                                  
         DC    C'IL',C'5MAG  '     INTERACTIVE - OLD                            
         DC    C'LL',C'5MAG  '     SOCIAL - OLD                                 
         DC    C'NL',C'5NEW  '     NEWSPAPERS - OLD                             
         DC    C'OL',C'5OUT  '     OUTDOOR - OLD                                
         DC    C'BL',C'5MAG  '     MOBILE - OLD                                 
         DC    C'VL',C'5MAG  '     NAT. VIDEO - OLD                             
         DC    C'WL',C'5MAG  '     LOC. VIDEO - OLD                             
         DC    C'DL',C'5MAG  '     DIG. AUDIO - OLD                             
         DC    X'FFFF'                                                          
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETFAXBF DS    0H                  INIT FAX BUFFERS                             
         XC    0(200,RF),0(RF)     RF POINTS TO FAX BUFFER                      
         MVC   0(5,RF),=C'START'                                                
         LA    RF,4(RF)            1ST BYTE OF PQPLD IS SET IN OLPRINT          
         MVC   QLDESC,=C'IO-XNNNN   '                                           
         MVC   QLDESC+3(1),PRQMED                                               
         LH    R0,REFNO                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QLDESC+4(4),DUB                                                  
         MVC   RCJOB,QLDESC        SAVE 'JOB'                                   
*                                                                               
         MVC   QLMAKER(3),=C'P72'                                               
*                                                                               
         MVC   QLSUBID,=C'PIO'     PRINTPAK INSERTION ORDER                     
         MVI   QLEXTRA,X'FF'                                                    
         MVC   QLRETNL,=H'48'      48 HOURS RETAINED UNTIL PRINTED              
         MVC   QLRETND,=H'12'      12 HOURS AFTER PRINTING                      
         MVI   QLCLASS,C'G'        WESTERN UNION IS CLASS G                     
         BR    RE                                                               
         DROP  RF                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CLRMED   DS    0H                                                               
         XC    PRQMEDN,PRQMEDN                                                  
         FOUT  PRQMEDNH                                                         
         NI    PRQMEDH+4,X'DF'                                                  
         XC    PRQPUBN,PRQPUBN                                                  
         FOUT  PRQPUBNH                                                         
         NI    PRQPUBH+4,X'DF'                                                  
*                                                                               
CLRCLT   DS    0H                                                               
         XC    PRQCLTN,PRQCLTN                                                  
         FOUT  PRQCLTNH                                                         
         NI    PRQCLTH+4,X'DF'                                                  
*                                                                               
CLRPRD   DS    0H                                                               
         XC    PRQPRDN,PRQPRDN                                                  
         FOUT  PRQPRDNH                                                         
         NI    PRQPRDH+4,X'DF'                                                  
*                                                                               
         XC    PRQESTN,PRQESTN                                                  
         FOUT  PRQESTNH                                                         
         NI    PRQESTH+4,X'DF'                                                  
*                                                                               
         NI    PRQJOBH+4,X'DF'                                                  
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FNDPUB   NTR1                      GET PUB RECORD(S)- PUB CODE IN DUB           
         XC    PUBREC(50),PUBREC                                                
         MVC   WORK(64),KEY                                                     
         LA    R4,6                FOR KEY COMPARE EX INSTRUCTION               
*                                                                               
         CLI   ZEOPT,C'Y'                                                       
         BNE   *+8                                                              
         LA    R4,4                FOR EX INSTRUCTION                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),PRQMED                                                    
         MVC   KEY+1(6),DUB                                                     
         MVC   KEY+7(2),AGYALPHA                                                
         BAS   RE,HIGHPUB                                                       
         B     *+8                                                              
FP2      BAS   RE,SEQPUB                                                        
         EX    R4,KEYCOMP                                                       
         BNE   FP8                                                              
         CLC   KEY+7(2),KEYSAVE+7  CHECK AGENCY                                 
         BE    FP4                                                              
         CLC   KEY+7(2),=C'ZZ'                                                  
         BNE   FP2                                                              
         CLI   PAGYPROF+16,C'0'    TEST DEFAULT                                 
         BE    FP8                 NO                                           
*                                                                               
FP4      DS    0H                                                               
         CLI   KEY+9,X'81'                                                      
         BNE   FP6                                                              
         CLI   PUBREC,0                                                         
         BNE   FP10                                                             
         LA    R0,PUBREC                                                        
         ST    R0,AREC                                                          
         BAS   RE,GETPUB                                                        
         B     FP2                                                              
*                                                                               
FP6      DS    0H                                                               
         B     FP2                                                              
*                                                                               
FP8      DS    0H                                                               
         MVC   KEY(64),WORK                                                     
         CLI   PUBREC,0                                                         
         BE    FPNO                                                             
*                                                                               
FP10     DS    0H                                                               
*                                                                               
JUMPXIT1 XIT1                                                                   
*                                                                               
FPNO     DS    0H                                                               
         LA    R3,PUBERR                                                        
         B     ERROR                                                            
*                                                                               
KEYCOMP  CLC   KEY(0),KEYSAVE      EXECUTED                                     
*                                                                               
         PRINT GEN                                                              
*                                                                               
NXTELEM  ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         JE    NXTELX              CC IS EQUAL                                  
         CLI   0(R5),0                                                          
         JNE   NXTELEM                                                          
         LTR   R5,R5               CC IS NOT EQUAL                              
NXTELX   BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT NOGEN                                                            
INVERR   EQU   2                                                                
MEDERR   EQU   13                                                               
CLTERR   EQU   14                                                               
PRDERR   EQU   15                                                               
ESTERR   EQU   16                                                               
PUBERR   EQU   18                                                               
DATERR   EQU   20                                                               
CTLERR   EQU   20                                                               
JOBERR   EQU   53                                                               
ACCERR   EQU   55                                                               
CACCERR  EQU   207                                                              
NOTUPD   EQU   400      UPDATES NOT ALLOWED IN THIS ONLINE APPLICATION          
NOGRPJOB EQU   402      GROUP JOB CODES NOT ALLOWED                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
       ++INCLUDE POLINITL2         IN-LINE CODES                                
*                                                                               
       ++INCLUDE POLRTNS           IN-LINE CODES                                
*                                                                               
       ++INCLUDE POLDMGR           IN-LINE CODES                                
*                                                                               
       ++INCLUDE POLEXTS           IN-LINE CODES                                
*                                                                               
ERRORXT  DS    0H                  FOR ERROR NUMBERS GREATER THAN 255           
         L     RF,VCOMFACS                                                      
         L     RF,(CGETTXT-COMFACSD)(RF)                                        
         MVI   ERRAREA,X'FF'                                                    
         GOTO1 (RF),DMCB+12,(R3),0,(C'E',DMCB),0,0,0                            
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKSETUPR NTR1  BASE=*,LABEL=*      CK FOR EIO SETUP RECORD                      
*                                                                               
         MVC   WORK(25),KEY        SHOULD HAVE CLIENT KEY                       
         XC    KEY+7(25-7),KEY+7                                                
         MVI   KEY+3,SCHKRCDQ                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,PRTDIR,KEY,KEY                               
         CLC   KEY(L'SCHKKEY),KEYSAVE                                           
         BE    CKSET20                                                          
         MVC   KEY(25),WORK                                                     
         XC    KEY+4(25-4),KEY+7   TRY ALL CLIENT                               
         MVI   KEY+3,SCHKRCDQ                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,PRTDIR,KEY,KEY                               
         CLC   KEY(L'SCHKKEY),KEYSAVE                                           
         BE    CKSET20                                                          
*                                                                               
         MVC   KEY(25),WORK        RESTORE SEQUENCE                             
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,PRTDIR,KEY,KEY                               
         J     SETCCEQ                                                          
*                                                                               
CKSET20  GOTO1 DATAMGR,DMCB,GETREC,PRTFILE,KEY+27,PBUYREC,DMWORK                
*                                                                               
         MVC   KEY(25),WORK        RESTORE SEQUENCE                             
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,PRTDIR,KEY,KEY                               
*                                                                               
         LA    RE,PBUYREC+33                                                    
         USING SCHHDRD,RE                                                       
         CLI   SCHHDRCD,SCHHDRQ    FIRST EIO SETUP ELEM PRESENT?                
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   SCHEIO,C'N'                                                      
         JE    SETCCEQ                                                          
         OC    SCHACTVD,SCHACTVD   HAVE ACTIVATION DATE?                        
         BNZ   *+16                                                             
         CLI   SCHEIO,C'Y'                                                      
         JE    SETCCNEQ                                                         
         J     SETCCEQ                                                          
         CLC   BTODAY,SCHACTVD     TODAY IS >= THAN ACTIVATION DATE?            
         JNL   SETCCNEQ                                                         
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
EIO_YERR EQU   326                 INSERTIONS SET TO USE EIO                    
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKTRAFID NTR1  BASE=*,LABEL=*      CHECKING FOR TRAFFIC ID SIGN-ON              
*                                                                               
         LA    R0,PBUYREC+1500     INIT WK AREA (TEMP USE)                      
         LHI   R1,400*4                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               SAME AS XCEF, ONLY SHORTER                   
*                                                                               
         LA    R5,PBUYREC+1500                                                  
         USING CTIREC,R5                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,10(RA)      ID NUMBER                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',(R5),(R5)                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,CTIDATA                                                       
CKTRA10  CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                ELEM MUST BE PRESENT                         
         CLI   0(RE),CTAGYELQ      AGENCY ALPHA ID ELEMENT (X'06')              
         BE    CKTRA20                                                          
         SR    R1,R1                                                            
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     CKTRA10                                                          
*                                                                               
CKTRA20  DS    0H                                                               
         USING CTAGYD,RE                                                        
*                                                                               
         CLI   CTAGYIDT,CTAGYTTQ   TRAFFIC ID (C'T')?                           
         BNE   SETCCNEQ                                                         
         DROP  R5,RE                                                            
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL (SIGN-ON IS NOT TRAFFIC)           
         J     JUMPXIT1                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TRAFFACC NTR1  BASE=*,LABEL=*      CHECKING FOR TRAFFIC LIMIT ACCESS            
*                                                                               
         MVI   BYTE3,0             WILL RETURN CODE                             
*                                                                               
         MVI   ELCODE,X'50'        CLIENT TRAFFIC OFFICE ELEM CODE              
         LA    R5,PCLTREC+33                                                    
         CLC   ELCODE,0(R5)        FOUND IN FIRST ELEM?                         
         BE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         BNE   TRACCX              NO CLIENT TRAFFIC OFFICE CODE ELEM           
         MVC   BYTE3,2(R5)         RETURN CLT TRAFFIC OFFICE CODE               
*                                                                               
TRACCX   J     JUMPXIT1                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKCTFFAX NTR1  BASE=*,LABEL=*      CKING FOR FAX CODE ON CONTROL FILE           
*                                                                               
         MVC   CKCTFWK,KEY         SAVE KEY                                     
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTFXKEY,R4                                                       
         MVI   CTFXKTYP,CTFXEQU                                                 
         MVC   CTFXAGY,AGYALPHA                                                 
         MVC   CTFXCODE,SECONDFX+3                                              
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI  ',=C'CTFILE ',KEY,CKCTFIO               
*                                                                               
         MVC   KEY,CKCTFWK         RESTORE KEY                                  
*                                                                               
         CLC   CKCTFIO(18),KEYSAVE                                              
         JNE   SETCCNEQ            FAX CODE IS NOT ON FILE                      
         LA    R4,CKCTFIO                                                       
         LA    R6,CTFXEL1                                                       
         B     CKCTF40                                                          
*                                                                               
CKCTF20  ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
CKCTF40  CLI   0(R6),0                                                          
         JE    SETCCNEQ            FAX CODE IS NOT ON FILE                      
         CLI   0(R6),CTFX1ELQ                                                   
         JE    SETCCEQ             FAX CODE IS VALIDATED                        
         B     CKCTF20                                                          
*                                                                               
CKCTFWK  DS    CL32                LOCAL WORKING STORAGE AREA                   
CKCTFIO  DS    600C                CONTROL FILE IO AREA                         
*                                                                               
         LTORG                                                                  
         DROP  RB,R4                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE T413WKA           SCREEN AND WORKING STORAGE DESECTS           
         EJECT                                                                  
*                                                                               
       ++INCLUDE POLFILE                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE T413WKB                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE PGENGRP                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE FATWA                                                          
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPSRCHPARM                                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPERREQUS         (P)RINT SYSTEM ERROR MSG EQUATES             
         EJECT                                                                  
*                                                                               
       ++INCLUDE FASECRETD                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPMAPEQUS                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPGENSCH          EIO SETUP RECORD                             
         EJECT                                                                  
*                                                                               
* XTRAINFD DSECT                                                                
       ++INCLUDE FAXTRAINF                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'131T41301    07/17/18'                                      
         END                                                                    
