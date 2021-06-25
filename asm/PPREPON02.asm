*          DATA SET PPREPON02  AT LEVEL 059 AS OF 05/01/02                      
         TITLE 'PPON02- OGILVY CREATE BUYS FROM TAPE'                           
*PHASE PPON02A                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE PUBVAL                                                                 
*INCLUDE PPRTLOOK                                                               
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE CASHVAL                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
* KWAN 01/01    CORRECT DUPLICATED LABEL USE PROBLEMS (FOR PANAPT)              
*                                                                               
* BPLA 01/04/91 CHANGE LOG MOVED HERE AND GETINS INCLUDE REMOVED                
*               =V(GETINS) CHANGED TO GETINS                                    
*                                                                               
* ROSA 06/12/90 EDITIONS FOR PUB CASE SENSITIVE                                 
* ROSA 02/01/90 CREATING 66 ELEMENTS S/B 67 (I/O COMMENTS)                      
*                                                                               
* ROSA 10/17/89  ADD CODE TO INCREMENT LINE NUMBER ON DUP KEYS                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PPON02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPON02                                                         
*                                                                               
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING PPON02+4096,RA                                                   
*                                                                               
         L     R9,0(R1)                                                         
         USING PPWORKD,R9                                                       
*                                                                               
         L     RC,PPFILEC                                                       
         LR    R8,RC                                                            
         AH    R8,=H'4096'                                                      
         USING PPFILED,RC,R8                                                    
*                                                                               
         CLI   MODE,PROCREQ                                                     
         BE    OM10                                                             
*                                                                               
EQXIT    CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
EXIT     XIT1                                                                   
         SPACE 1                                                                
OM10     DS    0H                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(3,TODAY)                                  
         OPEN  (FILEIN,(INPUT))                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         OPEN  (FILEOUT,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         XC    OMREC(255),OMREC                                                 
******************************************                                      
*** READ AGENCY RECORD FOR AGY PROFILE                                          
******************************************                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
         MVI   KEY+2,C'N'                                                       
         MVI   KEY+3,1                                                          
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BNE   *-2                                                              
         LA    R5,PESTREC                                                       
         ST    R5,AREC                                                          
         GOTO1 GETPRT                                                           
         MVC   AGYPROF,PAGYPROF                                                 
         EJECT                                                                  
******************************************                                      
*** BEGIN PROCESSING INPUT FILE                                                 
******************************************                                      
OM20     DS    0H                                                               
         BAS   RE,GETFILE                                                       
         CLI   EOFSW,C'Y'                                                       
         BE    OM400A                                                           
**************************************                                          
* READ CLIENT HEADER                                                            
**************************************                                          
         XC    KEY,KEY                                                          
         MVC   KEY(3),=C'OMN'                                                   
         MVI   KEY+3,2                                                          
         MVC   KEY+4(3),OMRCLI                                                  
         OI    KEY+6,X'40'               ENSURE BIN 0=X'40                      
         CLC   PCLTREC(15),KEY                                                  
         BE    RDPROD                                                           
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    *+12                                                             
         BAS   RE,MISSCLI                                                       
         B     PERROR                                                           
         LA    R5,PCLTREC                                                       
         ST    R5,AREC                                                          
*                                                                               
         GOTO1 GETPRT           READ AND SAVE CLIENT HEADER                     
         SPACE 3                                                                
*************************************                                           
* READ PRODUCT                                                                  
*************************************                                           
RDPROD   MVI   KEY+3,6                                                          
         MVC   KEY+7(3),OMRPRD                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(16),KEYSAVE                                                  
         BE    *+12                                                             
         BAS   RE,MISSPRD                                                       
         B     PERROR                                                           
         SPACE 3                                                                
*************************************                                           
* READ ESTIMATE                                                                 
*************************************                                           
         MVI   KEY+3,7                                                          
         LA    R0,PESTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 =V(NUMVAL),DMCB,OMREST,3                                         
         MVC  KEY+10(2),DMCB+6                                                  
         MVC   OMBEST,DMCB+6                                                    
         CLC   KEY(16),PESTREC                                                  
         BE    DODATE                                                           
         GOTO1 HIGH                                                             
         CLC   KEY(16),KEYSAVE                                                  
         BE    ESTFOUND                                                         
**************************************************************                  
*   MISSING ESTIMATE // CLIENT AND PROD OK.   TRY TO CREATE A NEW EST           
*    ESTIMATE DUMMY TO BE CREATED BY O&M.  PRODUCT WILL BE AAX. WHERE           
*    X WILL BE THE UNIT DIGIT OF THE ESTIMATE.. EG MISSING EST 089              
*    WILL HAVE AN EST PRODUCT OF AA9. IF MISSING THEN ERROR                     
***************************************************************                 
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+7(2),=C'AA'                                                  
         MVC   KEY+9(1),OMREST+2       UNIT DIGIT                               
         GOTO1 HIGH                                                             
         CLC   KEY(16),KEYSAVE                                                  
         BE    DATESOK                                                          
ESTREST  MVC   KEY(25),KEYSAVE         RESTORE ORIGINAL KEY                     
         MVC   KEY+7(3),OMRPRD                 PRODUCT                          
         MVC   KEY+10(2),OMBEST                EST                              
         BAS   RE,MISSEST                                                       
         B     DODATE                                                           
***************************************************************                 
*  ENSURE START AND END DATES FOR DUMMY EST STRADDLE INSERT DATE                
***************************************************************                 
DATESOK  GOTO1 GETPRT                                                           
         CLI   QOPT2,C'Y'                                                       
         BNE   *+8                                                              
         BAS   RE,DDSEST                                                        
         BAS   RE,VERDATE                                                       
         BNE   ESTREST              RESTORE                                     
         MVC   PESTKPRD,OMRPRD     NEW PRODUCT                                  
         MVC   KEY(25),0(RF)                                                    
         XC    PESTCNTL(6),PESTCNTL  CLEAR CONTROL                              
         CLI   QOPT2,C'Y'                                                       
         BNE   *+8                                                              
         BAS   RE,DDSEST                                                        
         AP    ESTCOUNT,=P'1'                                                   
         AP    OUTCOUNT,=P'1'                                                   
         CLI   QOPT1,C'Y'                                                       
         BE    DODATE                                                           
         GOTO1 ADDPRT                                                           
         B     DODATE                                                           
         SPACE 2                                                                
ESTFOUND GOTO1 GETPRT                                                           
***************************************************************                 
* ENSURE INSERT DATE IS OK AND FALLS WITHIN EST START & END DATE                
***************************************************************                 
DODATE   BAS   RE,VERDATE                                                       
         BE    DOPUB                                                            
         BNE   DOPUB                                                            
****************************************************                            
*   VALIDATE PUB                                                                
****************************************************                            
DOPUB   LA     R5,PUBREC                                                        
         OC    OMRPUB,=CL20' '     FORCE UPPER CASE              BUG02          
         ST    R5,AREC                                                          
        XC     PUBREC(32),PUBREC                                                
*                                                                               
        LA     RF,15               MUST DETERMINE LENGTH                        
        LA     RE,OMRPUB+14         FOR PUBVAL                                  
CLIBLAN CLI    0(RE),C' '                                                       
        BH     RFLEN                                                            
        BCTR   RE,0                                                             
        BCT    RF,CLIBLAN                                                       
        BAS    RE,MISSPUB                                                       
        B      CKCOL                                                            
*                                                                               
RFLEN    DS    0H                                                               
         GOTO1 =V(PUBVAL),DMCB,((RF),OMRPUB),PUBKPUB                            
         CLI   DMCB,255                                                         
         BNE   *+12                                                             
         BAS   RE,MISSPUB                                                       
         B     CKCOL                                                            
         MVC   OMBPUB,PUBKPUB                                                   
         MVI   PUBKMED,C'N'                                                     
         MVC   PUBKAGY,=C'OM'                                                   
         MVI   PUBKCOD,X'81'                                                    
         MVC   KEY,PUBREC                                                       
         GOTO1 HIGHPUB                                                          
         CLC   KEY(16),KEYSAVE                                                  
         BE    *+12                                                             
         BAS   RE,MISSPUB                                                       
         B     CKCOL                                                            
*******READ PUB                                                                 
         GOTO1 READPUB                                                          
         SPACE 3                                                                
********************************************                                    
*  VALIDATE JOB NUMBER                                                          
********************************************                                    
         XC    OMBJOB,OMBJOB                                                    
         CLC   OMRADNO(6),=CL6' '                                               
         BE    CKCOL                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),=C'OMN'                                                   
         MVI   KEY+3,X'15'                                                      
         MVC   KEY+4(6),OMRCLI                                                  
         MVC   KEY+10(6),OMRADNO                                                
         MVC   OMBJOB,OMRADNO                                                   
         MVC   KEY+16(6),OMBPUB                                                 
LOOKAGN  GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    CKCOL                                                            
         CLC   KEY(16),KEYSAVE                                                  
         BNE   XJOB                                                             
         CLI   KEY+16,255         ALL DEFAULT                                   
         BE    CKCOL                                                            
         MVC   KEY+16(6),=6X'FF'                                                
         B     LOOKAGN                                                          
*                                                                               
*                                                                               
XJOB     BAS   RE,BADJOB                                                        
         SPACE 3                                                                
***************************************************                             
* VALIDATE COLUMNS INCH AND RATE                                                
***************************************************                             
CKCOL    CLI   OMRCOL+3,C'0'   NO FRACTIONS                                     
         BNE   CKCOLBAD                                                         
         GOTO1 =V(CASHVAL),DMCB,(1,OMRCOL),4                                    
         CLI   DMCB,0                                                           
         BE    CKINCH                                                           
CKCOLBAD BAS   RE,BADCOL                                                        
*                                                                               
CKINCH   MVC   OMBCOL,DMCB+4                                                    
         GOTO1 =V(CASHVAL),DMCB,(0,OMRINCH),5                                   
         CLI   DMCB,0                                                           
         BE    CKRATE                                                           
         BAS   RE,BADINCH                                                       
*                                                                               
CKRATE   MVC   OMBINCH,DMCB+4                                                   
         GOTO1 =V(CASHVAL),DMCB,(4,OMRRATE),9                                   
         CLI   DMCB,0                                                           
         BE    *+8                                                              
         BAS   RE,BADRATE                                                       
         L     RE,DMCB+4                                                        
         MH    RE,=H'10'                                                        
         ST    RE,DMCB+4                                                        
         MVC   OMBRATE,DMCB+4                                                   
*                                                                               
         CLI   ERROR,0            ANY ERRORS DETECTED IN RECORD                 
         BE    PUTREC1                                                          
*                                                                               
PERROR   BAS   RE,SETPRINT                                                      
         BAS   RE,PRINTERR                                                      
         XC    PRIORKEY,PRIORKEY                                                
         B     OM20                                                             
*                                                                               
PUTREC1  MVC   PRIORKEY,OMREC                                                   
         PUT   FILEOUT,OMREC                                                    
         B     OM20                                                             
*                                                                               
         EJECT                                                                  
*********************************************                                   
* RETRIEVE OUTPUT OF EDIT AND ADD RECORDS    *                                  
*********************************************                                   
         SPACE 1                                                                
OM400A   DS    0H                                                               
         MVI   EOFSW,0                                                          
         CLI   ERROR,255                                                        
         BNE   OM400                                                            
         MVC   P(37),=C'JOB ABORTED DUE TO ERRORS FOUND ABOVE'                  
         GOTO1 REPORT                                                           
         B     EXIT                END JOB                                      
*                                                                               
OM400    BAS   RE,GETINPUT                                                      
         CLI   EOFSW,C'Y'                                                       
         BE    ENDOFJOB                                                         
         AP    NWSCOUNT,=P'1'                                                   
         AP    OUTCOUNT,=P'1'                                                   
         XC    KEY,KEY                                                          
         MVC   KEY+1(6),OMBPUB                                                  
         MVI   KEY,C'N'                                                         
         MVC   KEY+7(2),=C'OM'                                                  
         MVI   KEY+9,X'81'                                                      
         CLC   KEY(20),PUBREC       IS PUB THERE                                
         BE    PUBTHERE                                                         
         GOTO1 HIGHPUB                                                          
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,GETREC,PUBFILE,KEY+27,PUBREC,DMWORK                 
*                                                                               
PUBTHERE DS    0H                                                               
*                                                                               
*                                                                               
OM402    DS    0H                                                               
**************************************************************                  
* BEGIN ADDING RECORDS TO FILE                                                  
**************************************************************                  
         MVC   KEY(3),=C'OMN'                                                   
         MVC   KEY+4(6),OMRCLI     CLIENT PROD                                  
         MVC   KEY+10(6),OMBPUB    PUB                                          
         MVC   KEY+16(3),OMBID                                                  
         MVI   KEY+3,X'20'                                                      
         MVC   KEY+19(2),OMBEST                                                 
         MVC   PBUYREC(L'KEY),KEY      JUST MISSING LINE NUMBER                 
         XC    PBUYREC+L'KEY(256),PBUYREC+L'KEY                                 
         OI    DMINBTS,8           PASS DELETED RECORDS                         
         GOTO1 HIGH                                                             
         CLC   KEY(24),KEYSAVE                                                  
         BE    DOSEQ                                                            
         MVI   PBUYKLIN,1                                                       
         B     LENOFREC                                                         
************************************************************                    
*  ASSIGN LINE NUMBER BASED ON EXISTING BUYS                                    
************************************************************                    
DOSEQ    MVC   PBUYKLIN,KEY+(PBUYKLIN-PBUYKEY)                                  
         OI    DMINBTS,8           PASS DELETED RECORDS                         
         GOTO1 SEQ                                                              
         CLC   KEY(24),KEYSAVE                                                  
         BE    DOSEQ                                                            
*                                                               CH01            
ADDLIN1  ZIC   RF,PBUYKLIN   WAS USING KEY  S/B PBUYKLIN        CH01            
*                                                               CH01            
*DDLIN1  ZIC   RF,KEY+(PBUYKLIN-PBUYKEY)                                        
         LA    RF,1(RF)                                                         
         STC   RF,PBUYKLIN                                                      
*                                                                               
* LENGTH OF REC                                                                 
*                                                                               
LENOFREC DS    0H                                                               
*                                                                               
*                                           +BILLIN+PAY+I/O ELEMENTS            
         MVI   PBDELEM,X'20'                                                    
         MVI   PBDELEM+1,116                                                    
         XC    PBUYCNTL(6),PBUYCNTL   CLEAR CONTROL AND DISK ADDRESS            
*                                                                               
* MAKE SURE ALL PACKKED FIELDS ARE SIGNED                                       
*                                                                               
         ZAP   PBDUNITS,=P'0'                                                   
         ZAP   PBDCOS,=P'0'                                                     
         ZAP   PBDCD,=P'0'                                                      
         ZAP   PBDACP,=P'0'                                                     
         ZAP   PBDPRCOS,=P'0'                                                   
         ZAP   PBDCLMS,=P'0'                                                    
         MVC   PBDELEM(2),=X'2074'    ID AND LENGTH                             
         MVC   PBDBUYDT,TODAY                                                   
         MVC   PBDJOB,OMBJOB                                                    
         MVI   PBDUIND,X'89'                                                    
         MVI   PBDCOSIN,C' '                                                    
         MVI   PBDCOSTY,C'U'                                                    
*                                                                               
         LA    RF,PBDSPACE+2                                                    
         MVC   PBDSPACE(2),OMRCOL                                               
         CLI   OMRCOL,C'0'                                                      
         BH    *+12                                                             
         MVC   PBDSPACE(1),OMRCOL+1                                             
         BCTR  RF,0                                                             
         MVI   0(RF),C'X'                                                       
         MVC   1(5,RF),OMRINCH                                                  
*                                                                               
         ICM   RF,15,OMBCOL                                                     
         CVD   RF,DMCB             DMCB                                         
******** DP    DUB,DMCB+6(2)                                                    
******** CP    DUB+6(2),=P'0'      SEE IF UNITS DIVISABLE BY COL                
******** BNE   NOCOL                                                            
******** CVD   RF,DMCB                                                          
         DP    DMCB(8),=P'10'                                                   
         ZAP   PBDCLMS,DMCB(6)     COLUMN                                       
         ZAP   DUB(8),PBDCLMS                                                   
         CVB   RF,DUB                                                           
         ST    RF,OMBCOL           SAVE WHOLE COLUMN                            
*                                                                               
NOCOL    DS    0H                                                               
         ICM   RF,15,OMBRATE                                                    
         CVD   RF,DUB                                                           
         ZAP   PBDCOS,DUB                                                       
*                                                                               
         ICM   RF,15,OMBINCH                                                    
         MVC   DUB(2),OMBCOL+2                                                  
         MH    RF,DUB              TO GET TOTAL INCHES                          
*                                                                               
         CVD   RF,DUB                                                           
         ZAP   PBDUNITS,DUB                                                     
*                                                                               
         MVC   PROGPRO,PESTPROF    ESTIMATE PROFILE                             
         OC    PROGPRO(2),PROGPRO  ANYTHING THERE                               
         BNZ   MOSE7               EXIT                                         
         MVC   PROGPRO,PCLTPROF    CLIENT PROFILE                               
         OC    PROGPRO(2),PROGPRO                                               
         BNZ   MOSE7                                                            
         MVC   PROGPRO,AGYPROF     DEFAULT TO AGENCY                            
*                                                                               
MOSE7    LR    R5,R2               POINT TO BILLED DATE/INV/                    
         ZIC   R0,1(R5)            MONTH                                        
***                                                                             
         L     R6,=V(DATCON)                                                    
         L     RF,ADDAY                                                         
         GOTO1 =V(RATELOOK),DMCB,PBUYREC,PUBREC,=H'0',PROGPRO,(RF),    +        
               (R6)                                                             
***NEW 8/30/89 (BAP)                                                            
         CP    PBDCD,=P'1'          CD LOOK-UP OF 0                             
         BNE   *+10                                                             
         ZAP   PBDCD,=P'0'                                                      
         CP    PBDACP,=P'1'         AC LOOK-UP OF 0                             
         BNE   *+10                                                             
         ZAP   PBDACP,=P'0'                                                     
         CLC   PBDTAX,=X'000001'     TAX LOOK-UP OF 0                           
         BNE   *+10                                                             
         XC    PBDTAX,PBDTAX                                                    
         GOTO1 GETINS,DMCB,PBUYREC,GROSS,PBUYKPRD                               
         BAS   RE,OMPAYEL           ADD PAY ELEMENT                             
         BAS   RE,OMBILEL           ADD BILL ELEMENT                            
         MVI   ELCODE,X'67'                                     BUG01           
         BAS   RE,OMBUYCOM          ADD BUY COMMENTS                            
         MVI   ELCODE,X'68'                                                     
         BAS   RE,OMPOSCOM          ADD POSITION COMMENT                        
         BAS   RE,OMIOEL            ADD INSERT ORDER ELEMENT                    
         BAS   RE,OM99EL            ADD ID                                      
         MVC   KEY,PBUYREC                                                      
         LA    R6,PBUYREC                                                       
         ST    R6,AREC                                                          
********************************************************                        
**** DETERMINE LENGTH OF BUY                           *                        
****                                                   *                        
         LA    RF,33                                   *                        
         LA    RE,PBDELEM                              *                        
ZICR0    ZIC   R0,1(RE)                                *                        
         AR    RE,R0                                   *                        
         AR    RF,R0                                   *                        
         CLI   0(RE),0                                 *                        
         BNE   ZICR0                                   *                        
         STH   RF,DUB              LENGTH OF BUY       *                        
         MVC   PBUYLEN,DUB                             *                        
********************************************************                        
*  ADD THE BUY                                                                  
*******************************************                                     
         CLI   QOPT1,C'Y'                                                       
         BE    NOADD2                                                           
**************************** ADD BUY AND DIRECTORY POINTER ********             
*                                                                  CH01         
*                                                                  CH01         
         GOTO1 ADDPRT                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DMCB,KEY      SAVE DISK ADDRESS                                  
*******************************************************************             
NOADD2   XC    KEY,KEY                                                          
         MVC   KEY(25),0(R6)                                                    
         MVI   KEY+3,X'21'                                                      
         MVC   KEY+7(6),10(R6)     PUB                                          
         MVC   KEY+13(3),7(R6)     PRD                                          
         MVC   KEY+25(2),27(R6)                                                 
         MVC   KEY+27(4),DMCB      DISK ADDRESS                                 
         CLI   QOPT2,C'Y'                                                       
         BNE   NOSHOWK                                                          
         MVC   P(14),=C'ALTERNATE KEY '                                         
         GOTO1 HEXOUT,DMCB,KEY,P+18,31                                          
         GOTO1 REPORT                                                           
NOSHOWK  CLI   QOPT1,C'Y'                                                       
         BE    NOADD1                                                           
         GOTO1 DATAMGR,DMCB,=C'DMADD ',=C'PRTDIR',KEY,KEY,DMWORK                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
NOADD1   EDIT  (B1,KEY+24),(3,P+90)                                             
         EDIT  (B4,GROSS),(15,P+96),2,COMMAS=YES                                
         CLI   QOPT1,C'Y'                                                       
         BNE   *+10                                                             
         MVC   P+111(15),=C'** EDIT MODE **'                                    
         L     RF,GROSS                                                         
         L     RE,TOTALD                                                        
         AR    RE,RF                                                            
         ST    RE,TOTALD                                                        
         BAS   RE,SETPRINT                                                      
         CLI   QOPT2,C'Y'                                                       
         BNE   OM400                                                            
         BAS   RE,DDSTRACE                                                      
         B     OM400                                                            
*                                                                               
*                                                                               
ENDOFJOB LA    R4,COUNTS                                                        
         LA    R5,NCOUNTS                                                       
         EDIT  (B4,TOTALD),(15,P+96),2,COMMAS=YES                               
         CLI   QOPT1,C'Y'                                                       
         BNE   OM420                                                            
         MVC   P(L'NOTIFY),NOTIFY                                               
         GOTO1 REPORT                                                           
*                                                                               
OM420    OI    3(R4),X'0F'                                                      
         UNPK  P(7),0(4,R4)                                                     
         MVC   P+9(20),4(R4)                                                    
         GOTO1 REPORT                                                           
         LA    R4,L'COUNTS(R4)                                                  
         BCT   R5,OM420                                                         
*                                                                               
         B     EXIT                                                             
NOTIFY   DC    C'**** THIS IS AN EDIT RUN - NOTIFY DDS WHEN DATA IS OK'         
         EJECT                                                                  
********************                                                            
* ERRORS                                                                        
*******************                                                             
SETPRINT NTR1                                                                   
         MVC   P(1),OMRMTYP                                                     
         MVC   P+4(3),OMRCLI                                                    
         MVC   P+9(3),OMRPRD                                                    
         MVC   P+15(3),OMREST                                                   
         MVC   P+21(15),OMRPUB                                                  
         MVC   P+39(8),OMRID                                                    
         MVC   P+50(4),OMRCOL                                                   
         MVC   P+57(5),OMRINCH                                                  
         MVC   P+65(9),OMRRATE                                                  
         MVC   P+77(8),OMRADNO                                                  
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 2                                                                
MISSCLI  MVC   P+3+132(3),XXX                                                   
MISSBF   MVI   ERROR,255                                                        
         BR    RE                                                               
MISSPRD  MVC   P+9+132(3),XXX                                                   
         B     MISSBF                                                           
MISSEST  MVC   P+15+132(3),XXX                                                  
         B     MISSBF                                                           
MISSPUB  MVC   P+21+132(15),XXX                                                 
         B     MISSBF                                                           
BADDATE  MVC   P+39+132(8),XXX                                                  
         B     MISSBF                                                           
BADCOL   MVC   P+50+132(4),XXX                                                  
         B     MISSBF                                                           
BADINCH  MVC   P+57+132(5),XXX                                                  
         B     MISSBF                                                           
BADRATE  MVC   P+65+132(9),XXX                                                  
         B     MISSBF                                                           
BADJOB   MVC   P+77+132(8),XXX                                                  
         B     MISSBF                                                           
         SPACE 2                                                                
XXX      DC    25C'X'                                                           
         SPACE 2                                                                
PRINTERR NTR1                                                                   
         MVI   NOGOSW,255                                                       
*******  MVI   SPACING,1                                                        
*******  GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************                                 
* CREATE DUMMY PAY ELEMENT                                                      
***********************************************                                 
         SPACE 1                                                                
OMPAYEL  NTR1                                                                   
         BE    EXIT                                                             
*                                                                               
         LA    R6,PBDELEM                                                       
OMPAY2   ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             TEST EOF                                     
         BNE   OMPAY2                                                           
*                                                                               
         USING PPAYELEM,R6                                                      
*                                                                               
         XC    0(24,R6),0(R6)      CLEAR ELEM LEN + 2                           
         MVI   0(R6),X'25'                                                      
         MVI   1(R6),22                                                         
         B     EXIT                                                             
         EJECT                                                                  
*******************************************                                     
*       CREATE POSITION COMMENT           *                                     
*******************************************                                     
         SPACE 1                                                                
OMPOSCOM NTR1                                                                   
         LA   R2,OMRPOSLN                                                       
         BAS  RE,DOCOMM                                                         
         B    EXIT                                                              
         EJECT                                                                  
*******************************************                                     
*       CREATE BUY COMMENT ELEMENTS       *                                     
*******************************************                                     
         SPACE 1                                                                
OMBUYCOM NTR1                                                                   
         LA   R2,OMRCOML1                                                       
         BAS  RE,DOCOMM                                                         
         LA   R2,OMRCOML2                                                       
         BAS  RE,DOCOMM                                                         
         LA   R2,OMRCOML3                                                       
         BAS  RE,DOCOMM                                                         
         LA   R2,OMRCOML4                                                       
         BAS  RE,DOCOMM                                                         
         B    EXIT                                                              
****                                                                            
****                                                                            
****                                                                            
DOCOMM   CLI  0(R2),C' '             CONVERT BLANKS TO 0                        
         BNE  *+8                                                               
         OI   0(R2),X'F0'                                                       
         CLI  1(R1),C' '                                                        
         BNE  *+8                                                               
         OI   1(R2),X'F0'                                                       
         CLC  0(2,R2),=C'01'          LENGTH OF COMMENT                         
         BLR  RE                                                                
         OI   1(R2),X'F0'                                                       
         PACK DUB,0(2,R2)                                                       
         CVB  RF,DUB                                                            
         CLI   ELCODE,X'66'                                                     
         BNE   MBX68                                                            
         CH    RF,=H'46'                                                        
         BL    R6PDELEM                                                         
         LA    RF,45                                                            
         B     R6PDELEM                                                         
MBX68    CH   RF,=H'36'                                                         
         BL    *+8                                                              
         LA    RF,35                                                            
R6PDELEM LA    R6,PBDELEM                                                       
POSEL2   ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             TEST EOR                                     
         BNE   POSEL2                                                           
*                                                                               
         USING PCOMELEM,R6                                                      
*                                                                               
         XC    0(52,R6),0(R6)    CLEAR ELEM LEN + 2   FIXED 8/29/89             
         MVC   0(1,R6),ELCODE                                                   
         LA    RF,2(RF)          ADD ID AND LENGTH BYTES                        
         STC   RF,1(R6)                                                         
         SH    RF,=H'3'                                                         
         EX    RF,*+6                                                           
         BR    RE                                                               
         MVC   2(0,R6),2(R2)                                                    
*                                                                               
         EJECT                                                                  
*******************************************                                     
* CREATE INSERTION ORDER ELEMENT                                                
*******************************************                                     
         SPACE 1                                                                
OMIOEL   NTR1                                                                   
*                                                                               
         LA    R6,PBDELEM                                                       
BIOEL2   ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             TEST EOR                                     
         BNE   BIOEL2                                                           
*                                                                               
         USING PIOELEM,R6                                                       
*                                                                               
         XC    0(52,R6),0(R6)    CLEAR ELEM LEN + 2   FIXED 8/29/89             
*                                                     B.A.P (WAS 82)            
         MVI   0(R6),X'70'                                                      
         MVI   1(R6),50            FIXED 8/29/89 - B.A.P. (WAS 80)              
         B     EXIT                                                             
OM99EL   NTR1                                                                   
*                                                                               
         LA    R6,PBDELEM                                                       
B99EL2   ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             TEST EOR                                     
         BNE   B99EL2                                                           
*                                                                               
         USING PIOELEM,R6                                                       
*                                                                               
         XC    0(8,R6),0(R6)      CLEAR ELEM LEN + 2                            
         MVI   0(R6),X'99'                                                      
         MVI   1(R6),06                                                         
         MVI   2(R6),255                                                        
         MVC   3(3,R6),TODAY                                                    
         B     EXIT                                                             
******************************************************                          
*  VERIFY INSERT DATE                                                           
*****************************************************                           
VERDATE  NTR1                                                                   
         OI    OMRID,X'F0'                                                      
         GOTO1 =V(DATVAL),DMCB,OMRID,WORK                                       
         CLI   DMCB+3,0            ERROR                                        
         BNE   LAPEST                                                           
         CLI   P+15+132,C'X'       WAS EST IN ERROR                             
         BE    EQXIT               Y-NO START & END DATE CHECK                  
         BAS   RE,BADDATE                                                       
         B     EQXIT                                                            
LAPEST   CLI   P+15+132,C'X'       WAS EST IN ERROR                             
         BE    EQXIT               Y-NO START & END DATE CHECK                  
LAPESTT  GOTO1 =V(DATCON),DMCB,(4,OMRID),(3,OMBID)                              
         GOTO1 =V(DATCON),DMCB,(0,PESTST),(3,DUB)                               
         CLC   DUB(3),OMBID                                                     
         BH    BADD                                                             
         GOTO1 =V(DATCON),DMCB,(0,PESTEND),(3,DUB)                              
         CLC   DUB(3),OMBID                                                     
         BL    BADD                                                             
         B     EQXIT                                                            
BADD     BAS   RE,BADDATE                                                       
         B     NEQXIT                                                           
         SPACE 1                                                                
*******************************************                                     
* CREATE BILL ELEMENT                                                           
*******************************************                                     
         SPACE 1                                                                
OMBILEL  NTR1                                                                   
*                                                                               
         LA    R6,PBDELEM                                                       
BILEL2   ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             TEST EOR                                     
         BNE   BILEL2                                                           
*                                                                               
         USING PBILELEM,R6                                                      
*                                                                               
         XC    0(25,R6),0(R6)      CLEAR ELEM LEN + 2                           
         MVI   0(R6),X'26'                                                      
         MVI   1(R6),23                                                         
         MVC   2(3,R6),OMRPRD      INSERT PRODUCT                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************                      
**********************************************************                      
         SPACE 2                                                                
OMTRACE  NTR1                                                                   
         MVC   P(59),OMREC                                                      
         GOTO1 HEXOUT,DMCB,OMBEST,P+70,29,=C'TOG'                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
DDSEST   NTR1                                                                   
         CLI   QOPT2,C'Y'                                                       
         BNE   EXIT                                                             
         GOTO1 HEXOUT,DMCB,PESTREC,P,60,=C'TOG'                                 
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 HEXOUT,DMCB,PESTREC+60,P,60                                      
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 2                                                                
DDSTRACE NTR1                                                                   
         CLI   QOPT2,C'Y'                                                       
         BNE   EXIT                                                             
         GOTO1 HEXOUT,DMCB,PBUYKEY,P,25,=C'TOG'                                 
         GOTO1 (RF),(R1),PBUYLEN,P+51,2                                         
         GOTO1 (RF),(R1),PBUYCNTL,P+58,6                                        
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 HEXOUT,DMCB,PBDELEM,P+5,60                                       
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,PBDELEM+60,P+7,56                                    
         GOTO1 REPORT                                                           
*                                                                               
         LA    R6,PBDELEM                                                       
*                                                                               
DDSTR2   ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    EXIT                                                             
         ZIC   R0,1(R6)            GET CURRENT ELEMENT LENGTH                   
         GOTO1 HEXOUT,DMCB,(R6),P+5,(R0),=C'TOG'                                
*                                                                               
DDSTR4   DS    0H                                                               
         GOTO1 REPORT                                                           
         B     DDSTR2                                                           
*                                                                               
DDSTRMVC MVC   P+10(0),2(R6) *EXECUTED*                                         
         SPACE 2                                                                
* ON ENTRY R1 POINTS TO RECORD *                                                
         SPACE 1                                                                
GETFILE  NTR1                                                                   
*                                                                               
         GET   FILEIN,OMREC                                                     
GETFILEX B     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
GETINPUT NTR1                                                                   
         GET   INPUT,OMREC                                                      
         B     EXIT                                                             
*                                                                               
*                                                                               
ENDIN    CLOSE FILEIN                                                           
         CLOSE FILEOUT                                                          
         OPEN  (INPUT,(INPUT))                                                  
        XC     PUBREC(32),PUBREC                                                
         MVI   EOFSW,C'Y'                                                       
         B     EXIT                                                             
EOJ      CLOSE INPUT                                                            
         MVI   EOFSW,C'Y'                                                       
         B     EXIT                                                             
FMTKEY   NTR1                                                                   
         LA    R4,P+58                                                          
         GOTO1 HEXOUT,DMCB,(R5),(R4),1      AGY/MEDIA                           
         MVC   3(2,R4),1(R5)                CLIENT                              
         MVC   6(3,R4),7(R5)                PRODUCT                             
         SR    R0,R0                                                            
         ICM   R0,3,10(R5)                  ESTIMATE                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  10(4,R4),DUB                                                     
         MVI   14(R4),C'-'                                                      
         ZIC   R0,13(R5)                    LINE                                
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  15(2,R4),DUB                                                     
         ICM   R0,1,19(R5)         TEST SUBLINE PRESENT                         
         BZ    FMTKEY4                                                          
         MVI   17(R4),C'/'                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  18(2,R4),DUB                                                     
*                                                                               
FMTKEY4  GOTO1 HEXOUT,DMCB,14(R5),21(R4),6  OM PUB                              
*                                                                               
         CLI   ERRFLAG,C'P'                                                     
         BNE   EXIT                                                             
         GOTO1 HEXOUT,DMCB,SRCHPUB,34(R4),6   SEARCH PUB                        
         B     EXIT                                                             
         EJECT                                                                  
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=FB,MACRF=GM,EODAD=ENDIN             
*                                                                               
         SPACE 1                                                                
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=FB,MACRF=PM,              X        
               BLKSIZE=OMLENR*8,LRECL=OMLENR                                    
*                                                                               
         PRINT GEN                                                              
INPUT    DCB   DDNAME=INPUT,DSORG=PS,RECFM=FB,MACRF=GM,EODAD=EOJ,      X        
               BLKSIZE=OMLENR*8,LRECL=OMLENR                                    
         PRINT  NOGEN                                                           
NET      DS    F                                                                
         DS    0D                                                               
COUNTS   DS    0CL24                                                            
MAGCOUNT DC    PL4'0',CL20'MAGAZINE RECS OUT'                                   
NWSCOUNT DC    PL4'0',CL20'NWSPAPER RECS OUT'                                   
ESTCOUNT DC    PL4'0',CL20'EST HEADERS ADDED'                                   
OUTCOUNT DC    PL4'0',CL20'TOTAL RECORDS OUT'                                   
NCOUNTS  EQU   ((*-COUNTS)/24)                                                  
SVOMKEY  DS    CL21                                                             
ERRFLAG  DC    X'00'                                                            
EOFSW    DC    X'00'                                                            
ELCODE   DC    X'0'                                                             
SRCHPUB  DS    XL6                                                              
SRCHPRD  DS    XL7                                                              
         DS    0D                                                               
         DC    CL8'*OMREC*'                                                     
OMREC    DS    CL306                                                            
*                                                                               
         ORG   OMREC                                                            
OMRMTYP  DC    CL1'P'              MEDIA TYPE                                   
OMRCLI   DC    CL3'AMD'            CLIENT CODE                                  
OMRPRD   DC    CL3'AA '            PRODUCT CODE                                 
OMREST   DC    CL3'002'            ESTIMATE                                     
OMRPUB   DC    CL15'11034950'     PUBCODE                                       
OMRKEYL  EQU   *-OMRMTYP                                                        
OMRID    DC    CL8'01/01/89'       INSERT DATE                                  
OMRCOL   DC    CL4'03.0'           COLUMNS                                      
OMRINCH  DC    CL5'15.00'          INCHES                                       
OMRRATE  DC    CL9'0123.8888'      RATE                                         
OMRADNO  DC    CL8'ORANGE'         AD NUMBER                                    
OMRPOSLN DC    C'12'                LENGTH OF POSITION                          
OMRPOS   DC    CL35'123456789012'    POSITION                                   
OMRCOML1 DC    C'08'                COMMENT 1 LEN                               
OMRCOM1  DC    CL45'THIS IS 8 POSITIONS'                                        
OMRCOML2 DC    C'14'                COMMENT 2 LEN                               
OMRCOM2  DC    CL45'FORTEEN POSITONS'                                           
OMRCOML3 DC    C'10'                COMMENT 3 LEN                               
OMRCOM3  DC    CL45'THIS IS 10 POSITIONS'                                       
OMRCOML4 DC    C'54'                COMMENT 4 LEN                               
OMRCOM4  DC    CL45'FIFTEEN POSITONS'                                           
OMRLEN   EQU   *-OMREC                                                          
*************** ADDED FIELDS ***********                                        
OMBEST   DS    XL2                 CONVERTED EST NO                             
OMBPUB   DS    XL6                           PUB NUMBER                         
OMBID    DS    XL3                           INSERT DATE                        
OMBCOL   DS    F                             COLUMN                             
OMBINCH  DS    F                             INCHES                             
OMBRATE  DS    F                             RATE                               
OMBJOB   DS    CL6                           JOB NUMBER RIGHT JUSTIFIED         
*                                                                               
         ORG                                                                    
*                                                                               
OMLENR   EQU   *-OMREC                                                          
PRIORKEY DS    CL25                                                             
         ORG   PRIORKEY                                                         
PRIMTYP  DS    CL1                 MEDIA TYPE                                   
PRICLI   DS    CL3                 CLIENT CODE                                  
PRIPRD   DS    CL3                 PRODUCT CODE                                 
PRIEST   DS    CL3                 ESTIMATE                                     
PRIPUB   DS    CL15                PUBCODE                                      
         ORG                                                                    
*                                                                               
         LTORG                                                                  
NOGOSW   DC    X'0'                                                             
TODAY    DC    XL3'0'                                                           
ERROR    DC    X'0'                                                             
PROGPRO  DC    XL16'0'                                                          
AGYPROF  DC    XL16'0'                                                          
TOTALD   DC    F'0'                                                             
         EJECT                                                                  
OMRECD   DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'059PPREPON02 05/01/02'                                      
         END                                                                    
*          DATA SET PPREPON02  AT LEVEL 053 AS OF 10/17/89                      
