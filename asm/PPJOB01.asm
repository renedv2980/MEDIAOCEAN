*          DATA SET PPJOB01    AT LEVEL 088 AS OF 07/07/14                      
*PHASE T40F01B                                                                  
*INCLUDE SRCHCALL                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPJOB01 - PRINTPAK JOB FILE - JOB RECORD MODULE'                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 05/21/14 Allow 11 or 12 characters Ad-ID                                 
*                                                                               
* SMYE 4/05-9/05 ALLOW AD-ID ALONE AS "KEY" ENTRY                               
*                                                                               
* SMYE  01/05   AD ID FIELD ADDED (PAIRED WITH JOBCODE)                         
*                                                                               
* KWAN 10/15/01 BROWSE FUNCTION FOR SPACE DESCRIPTION                           
*                                                                               
* KWAN 09/21/01 REORGANIZED PPJOBWRK AND VALIDATE SPACE DESCRIPTION             
*                                                                               
*  BPLA  06/01   PRODUCTION JOB ADDED                                           
*                                                                               
* SMYE 04/17/01  OC SPACES TO PJOBPLIS (LIST CODE FROM PUB LIST REC)            
*                TO ELIMINATE POSSIBLE NULLS IN 2ND AND /OR 3RD POS'N.          
*                                                                               
* SMYE 02/19/98  CODE FOR PUB LIST AND FIX BILLING REP DISPLAY                  
*                                                                               
* BPLA 5/96      ALSO ALLOW "ADF" OR "AD" TO RETURN TO AD SCREEN                
*                                                                               
* SMYE 12/07/95  CHANGED VDTCNV TO VDATCON WITH NEW PARAM'S                     
*                                                                               
* BOBY 06/16/94  CODE FOR BILLING REP                                           
* BPLA 11/13/92  CODE FOR FSI FEATURE                                           
*                ALSO BILLING CONTACT FOR EVERONE                               
*                                                                               
* BPLA 9/11/91   CODE FOR REP SEARCHING (PRODUCTION HOUSE)                      
*                                                                               
T40F01   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T40F01,RR=RE                                                   
*                                                                               
         LA    R8,4095(RB)                                                      
         LA    R8,1(R8)                                                         
         USING T40F01+4096,R8                                                   
*                       ** NOTE USE OF R8 AS SECOND BASE REGISTER               
*                                                                               
         ST    RE,RELO                                                          
         B     *+8                                                              
RELO     DS    F                                                                
*                                                                               
*                                                                               
         SPACE 2                                                                
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T40FFFD,RA                                                       
*                                                                               
         CLI   SCRNTYP,1           TEST HAVE JOB SCREEN                         
         BE    JFM2                YES                                          
*                                  NO - FETCH SCREEN                            
         GOTO1 VCALLOV,DMCB,JOBLAST,X'D9040FFE'                                 
*                                                                               
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   SCRNTYP,1                                                        
         MVI   CHGSW,0                                                          
         MVI   NEWSCRN,C'N'                                                     
************                                                                    
********  BILLING CONTACT NOW FOR EVERYONE   11/13/92                           
************                                                                    
******   FSI ONLY FOR HDTO AND SJR                                              
******   WHILE TESTING                                                          
******                                                                          
****     CLC   AGYALPHA,=C'HD'         HDTO                                     
****     BE    JFM2                                                             
****     CLC   AGYALPHA,=C'SJ'         OR SJR                                   
****     BE    JFM2                                                             
****     XC    JOBFSIN,JOBFSIN         CLEAR FSI ANNO                           
****     FOUT  JOBFSINH                                                         
****     XC    JOBFSI,JOBFSI                                                    
****     OI    JOBFSIH+1,X'20'        AND PROTECT FSI                           
****     FOUT  JOBFSIH                                                          
*                                                                               
JFM2     DS    0H                                                               
         CLC   =C'ADD',JOBACT                                                   
         BE    ADDR                                                             
         CLC   =C'CHA',JOBACT                                                   
         BE    CHA                                                              
         CLC   =C'DIS',JOBACT                                                   
         BE    DIS                                                              
         CLC   =C'JOB',JOBACT                                                   
         BE    DIS                                                              
         CLC   =C'ADF',JOBACT      ALSO ALLOW ADF OR AD                         
         BE    DIS                 TO RETURN TO AD SCREEN                       
         OI    JOBACT+2,C' '                                                    
         CLC   =C'AD ',JOBACT                                                   
         BE    DIS                                                              
         LA    R3,ACTERR                                                        
         LA    R2,JOBACTH                                                       
         B     ERROR                                                            
         EJECT                                                                  
*                                  ADD ROUTINES                                 
ADDR     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(14),SVMED     M/C/CLT/PRD/JOB                              
         MVI   KEY+3,X'15'                                                      
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         LA    R2,JOBJOBH                                                       
         CLI   SVJOB,X'FF'         AD-ID ONLY ?                                 
         BE    ADDR2               YES - USE 'C1' PASSIVE KEY                   
         CLI   SVJOB,C' '          DO WE HAVE A JOB CODE ?                      
         BH    ADDR4               YES - USE "REGULAR" KEY                      
ADDR2    DS    0H                                                               
         MVI   KEY+3,PADIKRCQ      'C1' PASSIVE KEY CODE                        
         MVC   KEY+10(12),SVADID   AD-ID                                        
         LA    R2,JOBADIDH                                                      
ADDR4    DS    0H                                                               
         BAS   RE,HIGH                                                          
         NI    DMINBTS,X'F7'       DO NOT PASS DELETES                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ADD2                                                             
         LA    R3,ADDERR                                                        
         B     ERROR                                                            
*                                                                               
ADD2     DS    0H                                                               
         CLI   NEWSCRN,C'N'                                                     
         BNE   ADD4                                                             
         LA    R2,JOBCAP1H                                                      
ADD2E    MVC   JOBMSG(L'EJBMSG),EJBMSG                                          
         FOUT  JOBMSGH                                                          
         B     EXIT                                                             
*                                                                               
ADD4     DS    0H                                                               
         CLI   JOBADIDH+5,0        ANYTHING IN AD ID ?                          
         BE    ADD4E               NO                                           
         CLI   JOBJOBH+5,0         ANYTHING IN AD CODE ?                        
         BE    ADD4E               NO                                           
         MVC   JOBMSG(L'BOTHMSG),BOTHMSG  ADDING BOTH AD ID & AD CODE           
         FOUT  JOBMSGH                                                          
         B     EXIT                                                             
ADD4E    DS    0H                                                               
         LA    RE,PJOBREC          CLEAR PJOBREC                                
         LA    RF,1000                                                          
         XCEF                                                                   
*                                                                               
         MVC   PJOBKAGY,AGYALPHA                                                
         MVC   PJOBKMED(14),SVMED                                               
         MVI   PJOBKRCD,X'15'                                                   
         MVC   PJOBRLEN,=H'288'                                                 
         MVC   PJOBELEM(2),=X'15FF'                                             
         MVC   PJOBDATE,BTODAY                                                  
         BAS   RE,JBEDIT           EDIT JOB SCREEN FIELDS                       
*                     NOW CHECK FOR AD ID AND DO PASSIVE KEY WORK               
         CLI   PJOBADID,C' '                                                    
         BNH   ADDRXAD             NO AD ID CODE - JUST ADD RECORD              
*                                                                               
         MVC   PJOBADDT,BTODAY     DATE AD ID CREATED                           
*                                                                               
         XC    KEY,KEY             DO PASSIVE KEY WORK                          
         LA    R4,KEY              SET X'C1' PASSIVE KEY                        
         USING PADIRECD,R4                                                      
         MVC   PADIKAGY,AGYALPHA   AGENCY                                       
         MVC   PADIKMED,SVMED      MEDIA                                        
         MVI   PADIKRCD,PADIKRCQ   RECORD CODE (X'C1')                          
         MVC   PADIKCLT,SVCLT      CLIENT                                       
         MVC   PADIKPRD,SVPRD      PRODUCT                                      
         MVC   PADIKADI,PJOBADID   AD ID                                        
*                                                                               
         MVC   WORK(25),PADIKEY    SAVE PASSIVE KEY                             
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTOR HIGH                READ PRTDIR FOR PASSIVE                      
*                                                                               
         NI    DMINBTS,X'FF'-X'08' RESET                                        
*                                                                               
         CLC   PADIKEY,KEYSAVE                                                  
         BE    ADDP10              HAVE PASSIVE KEY - SEE IF DELETED            
*                                                                               
         XC    KEY,KEY             NO PASSIVE KEY                               
         MVC   PADIKEY,KEYSAVE       RESTORE "READ" KEY                         
         GOTOR ADD                   ADD IT                                     
         B     ADDP30              CONTINUE                                     
*                                                                               
ADDP10   DS    0H                  PASSIVE KEY EXISTS                           
         TM    PADICNTL,X'80'      FLAGGED FOR DELETION ?                       
         BO    ADDP20              YES - OK TO REUSE                            
*                                  DUPLICATE AD ID - ERROR MESSAGE              
         LA    R3,ADDERR                                                        
         LA    R2,JOBADIDH         POINT TO AD ID                               
         BAS   RE,GETREC           GET RECORD PASSIVE KEY POINTS TO             
         CLI   PJOBKJOB,X'FF'      AD-ID ONLY ?                                 
         BE    ERROR               YES - "DUPLICATE ON ADD" ERROR               
         MVC   JOBMSG(L'DUPIDMSG),DUPIDMSG   SHOW JOB CODE OF DUP AD-ID         
         MVC   JOBMSG+L'DUPIDMSG(L'PJOBKJOB),PJOBKJOB                           
         FOUT  JOBMSGH             PJOBKJOB IS EXISTING JOB CODE                
         B     EXIT                                                             
*                                                                               
ADDP20   DS    0H                  UNDELETE PASSIVE KEY FOR REUSE               
         XC    PADICNTL(6),PADICNTL                                             
         GOTOR WRITE               REWRITE PASSIVE KEY                          
*                                                                               
         DROP  R4                                                               
*                                                                               
ADDP30   DS    0H                                                               
         CLI   SVJOB,X'FF'         AD-ID ONLY (NO JOB CODE) ?                   
         BE    ADDP30G                                                          
         CLI   SVJOB,C' '          DO WE HAVE A JOB CODE ?                      
         BH    ADDP50              YES                                          
*                                                                               
ADDP30G  DS    0H                                                               
         BRAS  RE,VNXTSQN#         GET NEXT SEQUENCE NUMBER                     
*                                                                               
ADDP50   DS    0H                                                               
         CLI   SVJOB,X'FF'         AD-ID ONLY (NO JOB CODE) ?                   
         BE    ADDP50D             YES                                          
         CLI   SVJOB,C' '          DO WE HAVE A JOB CODE ?                      
         BH    ADDP50G             YES                                          
ADDP50D  DS    0H                                                               
         XC    PJOBKJOB,PJOBKJOB   CLEAR                                        
         MVI   PJOBKJOB,X'FF'      "SPECIAL" JOB CODE (X'FFNNNN')               
         MVC   PJOBKJOB+1(2),QSQN#    NNNN PORTION OF ABOVE                     
ADDP50G  DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),PJOBREC     RESTORE KEY                                  
         BAS   RE,ADDREC           ADD RECORD                                   
         MVC   KEY(25),PJOBREC     RESTORE KEY                                  
         GOTOR HIGH                GET DIR KEY JUST WRITTEN                     
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         MVC   WORK+27(4),KEY+27   SAVE DA                                      
         XC    KEY,KEY                                                          
         MVC   KEY(25),WORK        WORK HAS 'C1' PASSIVE KEY                    
         GOTOR HIGH                GET PASSIVE KEY                              
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         MVC   KEY+27(4),WORK+27   ADD THE "SAVED" DISK ADDRESS                 
         GOTOR WRITE               REWRITE THE PASSIVE KEY                      
*                                                                               
         CLI   SVJOB,X'FF'         AD-ID ONLY (NO JOB CODE) ?                   
         BE    ADDP50P             YES - UPDATE 'C2' PASSIVE                    
         CLI   SVJOB,C' '          DO WE HAVE A JOB CODE ?                      
         BH    ADDRXIT             YES - DONE                                   
*                                                                               
ADDP50P  DS    0H                                                               
         XC    KEY,KEY             AD-ID ONLY 'C2' PASSIVE                      
         LA    R4,KEY                                                           
         USING PADNRECD,R4                                                      
         MVC   PADNKEY(PADNKJOB-PADNKEY),PJOBREC                                
         MVI   PADNKRCD,PADNKRCQ   'C2' PASSIVE                                 
         MVC   PADNKJOB,PJOBKJOB   "SPECIAL" JOB CODE (X'FFNNNN')               
         GOTOR HIGH                GET PASSIVE KEY                              
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         MVC   PADNDISK,WORK+27    ADD THE "SAVED" DISK ADDRESS                 
         GOTOR WRITE               REWRITE THE PASSIVE KEY                      
*                                                                               
         MVC   SVMED(PJOBKPUB-PJOBKAGY),PJOBKMED  RESET FOR INS REC             
*                                                 IN PPJOB02                    
         B     ADDRXIT             DONE                                         
*                                                                               
ADDRXAD  DS    0H                                                               
         BAS   RE,ADDREC           USED ONLY IF AD ID NOT ENTERED               
ADDRXIT  DS    0H                                                               
         XC    ADIDJOB,ADIDJOB     CLEAR                                        
         CLI   PJOBKJOB,X'FF'      AD-ID ONLY ?                                 
         BNE   *+10                NO                                           
         MVC   ADIDJOB,PJOBKJOB    SAVE ADIDJOB FOR USE IN REQ PROC             
*                                                                               
         MVI   CHGSW,1                                                          
         MVC   JOBMSG(L'JRAMSG),JRAMSG                                          
         FOUT  JOBMSGH                                                          
         LA    R2,JOBMEDH                                                       
         B     EXIT                                                             
         EJECT                                                                  
*                                  CHANGE ROUTINES                              
CHA      DS    0H                                                               
         CLI   CHGSW,1                                                          
         BE    CHA2                                                             
         LA    R3,CHGERR                                                        
         LA    R2,JOBACTH                                                       
         B     ERROR                                                            
*                                                                               
CHA2     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(14),SVMED     M/C/CLT/PRD/JOB                              
         MVI   KEY+3,X'15'                                                      
         CLI   SVJOB,X'FF'         AD-ID ONLY ?                                 
         BE    CHA2C               YES                                          
         CLC   SVJOB,SPACES        DO WE HAVE A JOB CODE                        
         BH    CHA2G               YES                                          
CHA2C    DS    0H                                                               
         MVI   KEY+3,PADIKRCQ      'C1' PASSIVE KEY CODE                        
         MVC   KEY+10(12),SVADID   AD-ID                                        
CHA2G    DS    0H                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
*SMY*    BE    *+6                                                              
*SMY*    DC    H'0'                RECORD DISAPPEARED                           
         BE    CHA2K                                                            
         LA    R3,CHGERR                                                        
         LA    R2,JOBACTH                                                       
         B     ERROR               DISPLAY MUST PRECEDE CHANGE                  
CHA2K    DS    0H                                                               
         BAS   RE,GETREC                                                        
         MVC   SVADID,PJOBADID       SAVE AD ID FIELD FROM RECORD               
         MVC   SVADDT,PJOBADDT         AND AD ID CREATE DATE                    
         MVC   SVJOBSPC(17),PJOBELEM+69                                         
         XC    PJOBELEM+2(253),PJOBELEM+2                                       
         BAS   RE,JBEDIT           EDIT JOB SCREEN FIELDS                       
         MVC   PJOBADDT,SVADDT     "RESTORE" AD ID CREATE DATE                  
*                                                                               
*                     NOW CHECK FOR AD ID AND DO PASSIVE KEY WORK               
*                                                                               
         CLC   SVADID,PJOBADID                                                  
         BE    CHAXAD              NO CHANGE - JUST PUT RECORD                  
*                                                                               
         LA    R3,105              "THIS DATA CANNOT BE CHANGED"                
         LA    R2,JOBADIDH                                                      
*                                                                               
CHA2M    DS    0H                                                               
         CLI   SVADID,C' '         DID WE HAVE AN EXISTING AD ID                
         BNH   CHA4                NO - OK TO ADD ONE                           
*                                                                               
         B     ERROR               CANNOT CHANGE                                
*                                                                               
CHA4     DS    0H                  WE HAVE A CHANGED AD ID                      
         MVC   WORKA(64),KEY       SAVE WHATEVER KEYS WE HAVE                   
         XC    KEY,KEY                                                          
*                                                                               
         LA    R4,KEY              SET PASSIVE KEY WITH SVADID                  
         USING PADIRECD,R4                                                      
         MVC   PADIKAGY,AGYALPHA   AGENCY                                       
         MVC   PADIKMED,SVMED      MEDIA                                        
         MVI   PADIKRCD,PADIKRCQ   RECORD CODE (X'C1')                          
         MVC   PADIKCLT,SVCLT      CLIENT                                       
         MVC   PADIKPRD,SVPRD      PRODUCT                                      
         MVC   PADIKADI,SVADID     "CURRENT" AD ID                              
*                                                                               
         MVC   WORK(25),PADIKEY    SAVE PASSIVE KEY                             
*                                                                               
         CLI   SVADID,C' '         DID WE HAVE AN EXISTING AD ID                
         BNH   CHANEWID            NO - WE HAVE A "NEW" AD ID                   
         CLI   PJOBADID,C' '       AD ID REMOVED ?                              
         BH    CHANEWID            NO - WE HAVE A CHANGED AD ID                 
         XC    PJOBADDT,PJOBADDT   CLEAR AD ID CREATED DATE                     
*                                  AD ID REMOVED SO DELETE EXISTING             
*                                    PASSIVE AND REWRITE JOBREC                 
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTOR HIGH                READ PRTDIR FOR PASSIVE                      
*                                                                               
         NI    DMINBTS,X'FF'-X'08' RESET                                        
*                                                                               
         CLC   PADIKEY,KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         TM    PADICNTL,X'80'      FLAGGED FOR DELETION ?                       
         BNO   *+6                 NO                                           
         DC    H'0'                MAY NOT BE DELETED ALREADY                   
         OI    PADICNTL,X'80'      FLAG FOR DELETION                            
*                                                                               
         GOTOR WRITE               REWRITE                                      
         MVC   KEY(64),WORKA       RESTORE KEYS                                 
         B     CHAXAD              DONE - PUT RECORD                            
*                                                                               
CHANEWID DS    0H                  WE HAVE A "NEW" AD ID                        
*                                                                               
         MVC   PJOBADDT,BTODAY     SET AD ID CREATED DATE                       
*                                                                               
         XC    KEY,KEY                                                          
*                                                                               
         MVC   PADIKEY,WORK        "RESET" PASSIVE KEY WITH                     
         MVC   PADIKADI,PJOBADID     "NEW" AD ID                                
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTOR HIGH                READ PRTDIR FOR PASSIVE                      
*                                                                               
         NI    DMINBTS,X'FF'-X'08' RESET                                        
*                                                                               
         CLC   PADIKEY,KEYSAVE                                                  
         BE    CHAN10              HAVE PASSIVE KEY - SEE IF DELETED            
*                                                                               
         XC    KEY,KEY             NO PASSIVE KEY                               
         MVC   PADIKEY,KEYSAVE       RESTORE "READ" KEY                         
         GOTOR ADD                   ADD IT                                     
         B     CHAN30              CONTINUE                                     
*                                                                               
CHAN10   DS    0H                  PASSIVE KEY EXISTS                           
         TM    PADICNTL,X'80'      FLAGGED FOR DELETION ?                       
         BO    CHAN20              YES - OK TO REUSE                            
*                                  DUPLICATE AD ID - ERROR MESSAGE              
         BAS   RE,GETREC           GET RECORD PASSIVE KEY POINTS TO             
         MVC   JOBMSG(L'DUPIDMS2),DUPIDMS2                                      
         CLI   PJOBKJOB,X'FF'      AD-ID ONLY ?                                 
         BE    CHAN12              YES - "AD ID ALREADY USED" ERROR             
         XC    JOBMSG,JOBMSG                                                    
         MVC   JOBMSG(L'DUPIDMSG),DUPIDMSG                                      
         MVC   JOBMSG+L'DUPIDMSG(L'PJOBKJOB),PJOBKJOB                           
CHAN12   FOUT  JOBMSGH             PJOBKJOB IS EXISTING JOB CODE                
         LA    R2,JOBADIDH         POINT TO AD ID                               
         B     EXIT                                                             
*                                                                               
CHAN20   DS    0H                  UNDELETE PASSIVE KEY FOR REUSE               
         XC    PADICNTL(6),PADICNTL                                             
         GOTOR WRITE               REWRITE PASSIVE KEY                          
*                                                                               
CHAN30   DS    0H                                                               
         MVC   KEY(64),WORKA       RESTORE KEYS                                 
         BAS   RE,PUTREC           REWRITE RECORD                               
         GOTOR HIGH                GET DIR KEY JUST WRITTEN                     
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         MVC   WORK+27(4),KEY+27   SAVE DA                                      
         XC    KEY,KEY                                                          
*                                                                               
         MVC   PADIKEY,WORK        "RESET" PASSIVE KEY WITH                     
         MVC   PADIKADI,PJOBADID     "NEW" AD ID                                
*                                                                               
         GOTOR HIGH                GET PASSIVE KEY                              
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         MVC   PADIDISK,WORK+27    ADD THE SAVED DISK ADDRESS                   
         GOTOR WRITE               REWRITE THE PASSIVE KEY                      
*                                                                               
         CLI   SVADID,C' '         DID WE HAVE AN EXISTING AD ID                
         BNH   CHAXIT              NO - DONE                                    
*                           AD ID CHANGED SO DELETE "OLD" PASSIVE               
         XC    KEY,KEY                                                          
*                                                                               
         MVC   PADIKEY,WORK        "RESET" PASSIVE KEY WITH                     
         MVC   PADIKADI,SVADID       "OLD" AD ID                                
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
*                                                                               
         GOTOR HIGH                READ PRTDIR FOR PASSIVE                      
*                                                                               
         NI    DMINBTS,X'FF'-X'08' RESET                                        
*                                                                               
         CLC   PADIKEY,KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         TM    PADICNTL,X'80'      FLAGGED FOR DELETION ?                       
         BNO   *+6                 NO                                           
         DC    H'0'                MAY NOT BE DELETED ALREADY                   
         OI    PADICNTL,X'80'      FLAG FOR DELETION                            
*                                                                               
         GOTOR WRITE               REWRITE                                      
         MVC   KEY(64),WORKA       RESTORE KEYS                                 
*                                                                               
         B     CHAXIT              DONE                                         
*                                                                               
         DROP  R4                                                               
CHAXAD   DS    0H                                                               
         BAS   RE,PUTREC                                                        
CHAXIT   DS    0H                                                               
         XC    ADIDJOB,ADIDJOB     CLEAR                                        
         CLI   PJOBKJOB,X'FF'      AD-ID ONLY ?                                 
         BNE   *+10                NO                                           
         MVC   ADIDJOB,PJOBKJOB    SAVE ADIDJOB FOR USE IN REQ PROC             
*                                                                               
         MVC   JOBMSG(L'JRCMSG),JRCMSG                                          
         FOUT  JOBMSGH                                                          
         LA    R2,JOBMEDH                                                       
         B     EXIT                                                             
         EJECT                                                                  
*                                  DISPLAY ROUTINES                             
DIS      DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(14),SVMED     M/C/CLT/PRD/JOB                              
         MVI   KEY+3,X'15'                                                      
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         LA    R2,JOBJOBH                                                       
         CLI   SVJOB,X'FF'         AD-ID ONLY ?                                 
         BE    DIS02               YES                                          
         CLC   SVJOB,SPACES        DO WE HAVE A JOB CODE                        
         BH    DIS04               YES                                          
DIS02    DS    0H                                                               
         MVI   KEY+3,PADIKRCQ      'C1' PASSIVE KEY CODE                        
         MVC   KEY+10(12),JOBADID  AD-ID                                        
         LA    R2,JOBADIDH                                                      
DIS04    DS    0H                                                               
         BAS   RE,HIGH                                                          
         NI    DMINBTS,X'F7'       DO NOT PASS DELETES                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    DIS20                                                            
         CLC   =C'DIS',JOBACT                                                   
         BNE   DIS60                                                            
         LA    R3,DISERR                                                        
DIS10    DS    0H                                                               
         B     ERROR                                                            
*                                                                               
DIS20    DS    0H                                                               
         TM    KEY+25,X'C0'                                                     
         BZ    DIS40                                                            
         LA    R3,DELERR                                                        
         B     DIS10                                                            
*                                                                               
DIS40    DS    0H                                                               
         BAS   RE,GETREC                                                        
         CLI   PJOBKJOB,X'FF'      AD-ID ONLY ?                                 
         BE    DIS44               YES                                          
         MVC   JOBJOB(L'PJOBKJOB),PJOBKJOB                                      
         MVC   SVJOB,PJOBKJOB      "REGULAR" AD CODE                            
         FOUT  JOBJOBH             XMIT AD CODE                                 
DIS44    BAS   RE,JBFMT                                                         
         MVI   CHGSW,1             SET TO ALLOW CHANGE NEXT TIME                
*                                                                               
         MVC   JOBMSG(L'JRDMSG),JRDMSG                                          
         FOUT  JOBMSGH                                                          
         LA    R2,JOBACTH                                                       
         XC    JOBACT,JOBACT                                                    
         FOUT  JOBACTH                                                          
         B     EXIT                                                             
*                                                                               
DIS60    DS    0H                                                               
         XC    JOBACT,JOBACT                                                    
         FOUT  JOBACTH                                                          
*                                                                               
         LA    R2,JOBACTH                                                       
         B     ADD2E                                                            
         EJECT                                                                  
*                                  FORMAT DATA ON SCREEN                        
JBFMT    DS    0H                                                               
         ST    RE,SAVRE                                                         
*                                  CAPTION                                      
         MVC   JOBCAP1,PJOBCAP1                                                 
         FOUT  JOBCAP1H                                                         
*                                  CAPTION LINE 2                               
         MVC   JOBCAP2,PJOBCAP2                                                 
         FOUT  JOBCAP2H                                                         
*                                  AD ID                                        
         MVC   JOBADID,PJOBADID                                                 
         FOUT  JOBADIDH                                                         
*                                  COPY NUMBER                                  
         MVC   JOBCPY,PJOBCPY                                                   
         FOUT  JOBCPYH                                                          
*                                  SPACE                                        
         MVC   JOBSPC,PJOBSPC                                                   
         FOUT  JOBSPCH                                                          
*                                  UNITS                                        
         XC    WORKA,WORKA                                                      
         CLI   PJOBUIND,C'P'                                                    
         BNE   JBF4                                                             
         MVC   WORKA(4),=C'PAGE'                                                
         B     JBF6                                                             
*                                                                               
JBF4     DS    0H                                                               
         CP    PJOBUNTS,=P'0'                                                   
         BE    JBF6                                                             
         CLI   PJOBUIND,X'89'                                                   
         BNE   JBF4B                                                            
         EDIT  PJOBUNTS,(6,WORKA),2,ALIGN=LEFT                                  
         B     JBF4D                                                            
JBF4B    EDIT  PJOBUNTS,(5,WORKA),ALIGN=LEFT                                    
*                                                                               
JBF4D    LA    R4,WORKA                                                         
         AR    R4,R0                                                            
         MVC   0(1,R4),PJOBUIND                                                 
         CLI   0(R4),C'I'                                                       
         BE    JBF5                                                             
         CLI   0(R4),X'89'                                                      
         BNE   JBF6                                                             
         OI    0(R4),X'40'                                                      
JBF5     MVI   1(R4),C'N'                                                       
*                                                                               
JBF6     DS    0H                                                               
         MVC   JOBLIN,WORKA                                                     
         FOUT  JOBLINH                                                          
*                                  COLUMNS                                      
         XC    WORKA,WORKA                                                      
         CP    PJOBCOLS,=P'0'                                                   
         BE    JBF8                                                             
         EDIT  PJOBCOLS,(4,WORKA),ALIGN=LEFT                                    
*                                                                               
JBF8     DS    0H                                                               
         MVC   JOBCOL,WORKA                                                     
         FOUT  JOBCOLH                                                          
*                                       LINES EXPANSION                         
         XC    WORKA,WORKA                                                      
         CP    PJOBTUNS,=P'0'                                                   
         BE    JBF10                                                            
         CLI   PJOBUIND,X'89'                                                   
         BNE   JBF9                                                             
         EDIT  PJOBTUNS,(6,WORKA+2),2,ALIGN=LEFT                                
         B     JBF9A                                                            
JBF9     EDIT  PJOBTUNS,(5,WORKA+2),ALIGN=LEFT                                  
*                                                                               
JBF9A    MVI   WORKA,C'='                                                       
         LA    R4,WORKA+2                                                       
         AR    R4,R0                                                            
         MVC   0(1,R4),PJOBUIND                                                 
         CLI   0(R4),X'89'                                                      
         BNE   *+8                                                              
         OI    0(R4),X'40'                                                      
         CLI   0(R4),C'I'                                                       
         BNE   *+8                                                              
         MVI   1(R4),C'N'                                                       
*                                                                               
JBF10    DS    0H                                                               
         MVC   JOBLXC,WORKA                                                     
         FOUT  JOBLXCH                                                          
*                                                                               
*                                  PREM                                         
         MVC   JOBPRM,PJOBPRM                                                   
         FOUT  JOBPRMH                                                          
*                                  BFD                                          
*                                  START DATE                                   
         XC    WORKA,WORKA                                                      
         OC    PJOBSTA,PJOBSTA                                                  
         BZ    JBF12                                                            
*        GOTO1 VDTCNV,DMCB,(1,PJOBSTA),(3,WORKA)                                
         GOTO1 VDATCON,DMCB,(3,PJOBSTA),(5,WORKA)                               
*                                                                               
JBF12    DS    0H                                                               
         MVC   JOBSTA,WORKA                                                     
         FOUT  JOBSTAH                                                          
*                                  END DATE                                     
         XC    WORKA,WORKA                                                      
         OC    PJOBEND,PJOBEND                                                  
         BZ    JBF14                                                            
*        GOTO1 VDTCNV,DMCB,(1,PJOBEND),(3,WORKA)                                
         GOTO1 VDATCON,DMCB,(3,PJOBEND),(5,WORKA)                               
*                                                                               
JBF14    DS    0H                                                               
         MVC   JOBEND,WORKA                                                     
         FOUT  JOBENDH                                                          
*                                  ALLOCATION                                   
         MVC   JOBALO,PJOBALO                                                   
         FOUT  JOBALOH                                                          
*                                  AGENCY SIGNATURE                             
         MVC   JOBSIG,PJOBSIG                                                   
         FOUT  JOBSIGH                                                          
*                                                                               
         MVC   JOBBLCC,PJOBBLCC      BILLING CONTACT                            
         FOUT  JOBBLCCH                                                         
*                                                                               
*                                                                               
         XC    JOBFILT,JOBFILT                                                  
         MVC   JOBFILT(6),PJOBFILT   FILTER                                     
         FOUT  JOBFILTH                                                         
*                                                                               
JBF15    XC    JOBFSI,JOBFSI                                                    
         MVC   JOBFSI(1),PJOBFSI                                                
         CLC   JOBFSI,=X'0000'                                                  
         BNE   *+8                                                              
         MVI   JOBFSI,C'N'                                                      
         FOUT  JOBFSIH                                                          
*                                                                               
JBF15X   DS    0H                                                               
         XC    JOBPROD,JOBPROD                                                  
         MVC   JOBPROD(4),PJOBPROD                                              
         FOUT  JOBPRODH                                                         
*                                                                               
         MVC   WORKA(64),KEY            SAVE JOB RECORD KEY                     
         XC    JOBPRON,JOBPRON                                                  
         OC    PJOBPROD,PJOBPROD                                                
         BZ    JBF16                                                            
         XC    KEY+4(21),KEY+4                                                  
         MVI   KEY+3,X'11'         REP RECORD                                   
         MVC   KEY+4(4),PJOBPROD                                                
         MVC   JOBPRON(22),=C'**RECORD NOT ON FILE**'                           
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   JBF16                                                            
         MOVE  (IOAREA2,500),IOAREA     SAVE JOB RECORD                         
         BAS   RE,GETREC                                                        
         MVC   JOBPRON,PREPNAME                                                 
         MOVE  (IOAREA,500),IOAREA2     RESTORE JOB RECORD                      
JBF16    DS    0H                                                               
         MVC   KEY(64),WORKA            RESTORE JOB RECORD KEY                  
         FOUT  JOBPRONH                                                         
*                                                                               
         XC    JOBPJOB,JOBPJOB          PRODUCTION JOB                          
         MVC   JOBPJOB(6),PJOBPJOB                                              
         FOUT  JOBPJOBH                                                         
*                                                                               
*        DISPLAY BILLING REP CODE                                               
*                                                                               
         XC    JOBBREP,JOBBREP                                                  
         MVC   JOBBREP(4),PJOBBREP                                              
         FOUT  JOBBREPH                                                         
*                                                                               
         MVC   WORKA(64),KEY            SAVE JOB RECORD KEY                     
         XC    JOBBRPN,JOBBRPN                                                  
         OC    PJOBBREP,PJOBBREP                                                
         BZ    JBF17                                                            
         XC    KEY+4(21),KEY+4                                                  
         MVI   KEY+3,X'11'         REP RECORD                                   
         MVC   KEY+4(4),PJOBBREP                                                
         MVC   JOBBRPN(22),=C'**RECORD NOT ON FILE**'                           
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   JBF17                                                            
         MOVE  (IOAREA2,500),IOAREA     SAVE JOB RECORD                         
         BAS   RE,GETREC                                                        
         MVC   JOBBRPN,PREPNAME                                                 
         MOVE  (IOAREA,500),IOAREA2     RESTORE JOB RECORD                      
JBF17    DS    0H                                                               
         MVC   KEY(64),WORKA            RESTORE JOB RECORD KEY                  
         FOUT  JOBBRPNH                                                         
*                                                                               
*        DISPLAY PUB LIST                                                       
*                                                                               
         XC    JOBPLIS,JOBPLIS                                                  
         OC    PJOBPLIS,SPACES                                                  
         MVC   JOBPLIS(3),PJOBPLIS                                              
         FOUT  JOBPLISH                                                         
         MVC   WORKA(64),KEY            SAVE JOB RECORD KEY                     
*                                                                               
         XC    JOBPLSN,JOBPLSN                                                  
         CLI   PJOBPLIS,C' '                                                    
         BNH   JBF18X                                                           
         XC    KEY+7(18),KEY+7                                                  
         MVI   KEY+3,X'17'         PUBLIST RECORD                               
         MVC   KEY+7(3),PJOBPLIS                                                
         MVC   JOBPLSN(22),=C'**RECORD NOT ON FILE**'                           
JBF18B   BAS   RE,HIGH                                                          
         CLC   KEY(10),KEYSAVE                                                  
         BE    JBF18G                                                           
         CLC   KEYSAVE+4(3),=C'ZZZ'   WAS I LOOKING FOR ALL CLIENTS ?           
         BE    JBF18X                 YES - RECORD NOT FOUND                    
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+4(3),=C'ZZZ'                                                 
         B     JBF18B              TRY FOR "ZZZ" CLIENT CODE LIST REC           
JBF18G   DS    0H                                                               
         MOVE  (IOAREA2,500),IOAREA     SAVE JOB RECORD                         
         BAS   RE,GETREC                                                        
         XC    JOBPLSN,JOBPLSN                                                  
         MVC   JOBPLSN(20),PJOBREC+40                                           
*            * (PUB LIST DESCRIPTION IS 40 BYTES PAST RECORD START) *           
         MOVE  (IOAREA,500),IOAREA2     RESTORE JOB RECORD                      
JBF18X   DS    0H                                                               
         MVC   KEY(64),WORKA            RESTORE JOB RECORD KEY                  
         FOUT  JOBPLSNH                                                         
*                                                                               
JBFX     DS    0H                                                               
         L     RE,SAVRE                                                         
         BR    RE                                                               
         EJECT                                                                  
*                                  EDIT JOB SCREEN FIELDS                       
JBEDIT   DS    0H                                                               
         ST    RE,SAVRE                                                         
*                                  CAPTION                                      
         LA    R2,JOBCAP1H                                                      
         LA    R3,MISSERR                                                       
         OC    JOBCAP1,SPACES                                                   
         CLC   JOBCAP1,SPACES                                                   
         BE    ERROR                                                            
         MVC   PJOBCAP1,JOBCAP1                                                 
         OC    JOBCAP2,SPACES                                                   
         MVC   PJOBCAP2,JOBCAP2                                                 
*                                  AD ID                                        
         LA    R2,JOBADIDH                                                      
         LA    R3,INVERR                                                        
         CLI   5(R2),0             ANYTHING THERE ?                             
         BE    JBE1                NO                                           
         CLI   5(R2),11                                                         
         JE    JBE0X                                                            
         CLI   5(R2),12                                                         
         JE    JBE0X                                                            
         J     ERROR               Must be 11 or 12 characters                  
*                                                                               
JBE0X    DS    0H                                                               
*                                                                               
         MVC   PJOBADID,JOBADID    MOVE TO RECORD                               
*                                                                               
JBE1     DS    0H                  COPY NUMBER                                  
         LA    R2,JOBCPYH                                                       
         OC    JOBCPY,SPACES                                                    
         CLC   JOBCPY,SPACES                                                    
         BNE   JBE2                                                             
         LA    R4,JOBCPY                                                        
         MVC   0(3,R4),SVCLT                                                    
         LA    R4,2(R4)                                                         
         CLI   0(R4),C' '                                                       
         BNH   *+8                                                              
         LA    R4,1(R4)                                                         
         MVI   0(R4),C'-'                                                       
         MVC   1(3,R4),SVPRD                                                    
         LA    R4,3(R4)                                                         
         CLI   0(R4),C' '                                                       
         BNH   *+8                                                              
         LA    R4,1(R4)                                                         
         MVI   0(R4),C'-'                                                       
         MVC   1(6,R4),SVJOB                                                    
         FOUT  (R2)                                                             
*                                                                               
JBE2     DS    0H                                                               
         MVC   PJOBCPY,JOBCPY                                                   
*                                  SPACE                                        
         LA    R2,JOBSPCH                                                       
         OC    JOBSPC,SPACES                                                    
         CLC   JOBSPC,SPACES                                                    
         BNE   JBE4                                                             
         CLI   SVMED,C'N'                                                       
         BNE   ERROR                                                            
*                                                                               
JBE4     DS    0H                                                               
         MVC   WORK(17),SVJOBSPC   PASS CURRENT SPC DESP FOR CHECKINGS          
         BRAS  RE,CKSPDESP         CHECKING SPACE DESP FIELD                    
         CHI   R3,SPDSPERR         SPACE DESCRIPTION NOT FOUND?                 
         BE    ERROR                                                            
*                                                                               
         MVC   PJOBSPC,JOBSPC                                                   
*                                  UNITS                                        
         LA    R2,JOBLINH                                                       
         ZAP   PJOBUNTS,=P'0'                                                   
         ZAP   PJOBCOLS,=P'0'                                                   
         ZAP   PJOBTUNS,=P'0'                                                   
         CLI   SVMED,C'N'          NEWSPAPERS ONLY                              
         BNE   JBE12                                                            
*                                                                               
         CLI   5(R2),0        TEST ANY LINES                                    
         BE    JBE11          NO - SKIP TO PREM                                 
         LA    R3,LINERR                                                        
         LA    R4,JOBLIN                                                        
         LR    R5,R4                                                            
JBE7     DS    0H                                                               
         CLI   0(R4),C'.'                                                       
         BE    JBE7A                                                            
         CLI   0(R4),C'9'                                                       
         BH    JBE8                                                             
         CLI   0(R4),C'0'                                                       
         BL    JBE8                                                             
         LA    R4,1(R4)                                                         
         B     JBE7                                                             
DUB2     DS    D                                                                
*****                                                                           
JBE7A    LA    R4,3(R4)                                                         
         SR    R5,R4                                                            
         LPR   R5,R5                                                            
         BNP   ERROR                                                            
         MVC   DUB2,DUB                     SAVE OFF DUB FOR LATER              
         GOTO1 VCASHVAL,DMCB,(2,JOBLIN),(R5)         COMPARISON                 
         OI    DMCB,0                                                           
         BZ    JBE7C                                                            
         B     ERROR                                                            
JBE7C    XC    FULL,FULL                                                        
         MVC   DUB,DUB2                     RESTORE DUB                         
         XC    DUB2,DUB2                                                        
         MVC   FULL,DMCB+4                                                      
         L     R5,FULL                                                          
         CVD   R5,DUB2                                                          
         MVC   PJOBUNTS,DUB2+5                                                  
         MVI   PJOBUIND,X'89'                                                   
         B     JBE10                                                            
*****                                                                           
JBE8     DS    0H                                                               
         SR    R5,R4                                                            
         LPR   R5,R5               R5 = NUMERIC LENGTH                          
         BNP   ERROR                                                            
         CH    R5,=H'5'                                                         
         BH    ERROR                                                            
         BCTR  R5,R0                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  PJOBUNTS,JOBLIN(0)                                               
         MVC   PJOBUIND,0(R4)                                                   
         CLI   0(R4),C'I'                                                       
         BE    JBE10                                                            
         CLI   0(R4),C'L'                                                       
         BE    JBE10                                                            
         MVI   PJOBUIND,C'L'                                                    
         CLI   0(R4),0                                                          
         BE    JBE10                                                            
         CLI   0(R4),X'40'                                                      
         BE    JBE10                                                            
         B     ERROR                                                            
*                                  COLS                                         
JBE10    DS    0H                                                               
         LA    R2,JOBCOLH                                                       
         LA    R3,COLERR                                                        
         CLI   5(R2),0                                                          
         BNE   JBE10B                                                           
         ZAP   PJOBTUNS,PJOBUNTS                                                
         B     JBE10D                                                           
JBE10B   DS    0H                                                               
         TM    4(R2),X'08'                                                      
         BZ    ERROR                                                            
         BAS   RE,PACK                                                          
         ZAP   PJOBCOLS,DUB                                                     
*                                                                               
         MP    DUB,PJOBUNTS        COLS X LINES                                 
         CP    DUB,=P'99999'                                                    
         BH    ERROR                                                            
         ZAP   PJOBTUNS,DUB                                                     
*                                                                               
JBE10D   DS    0H                                                               
         XC    WORKA,WORKA                                                      
         CLI   PJOBUIND,X'89'                                                   
         BNE   JBE10E                                                           
         EDIT  PJOBTUNS,(6,WORKA+2),2,ALIGN=LEFT                                
         B     JBE10F                                                           
JBE10E   EDIT  PJOBTUNS,(5,WORKA+2),ALIGN=LEFT                                  
*                                                                               
JBE10F   MVI   WORKA,C'='                                                       
         LA    R4,WORKA+2                                                       
         AR    R4,R0                                                            
         MVC   0(1,R4),PJOBUIND                                                 
         CLI   0(R4),X'89'                                                      
         BNE   JBE10G                                                           
         OI    0(R4),X'40'                                                      
JBE10G   CLI   0(R4),C'I'                                                       
         BNE   *+8                                                              
         MVI   1(R4),C'N'                                                       
*                                                                               
         MVC   JOBLXC,WORKA                                                     
         FOUT  JOBLXCH                                                          
*                                  PREMIUM                                      
JBE11    LA    R2,JOBPRMH                                                       
         LA    R3,PRMERR                                                        
         OC    JOBPRM,SPACES                                                    
         MVC   PJOBPRM,JOBPRM                                                   
         CLC   JOBPRM,SPACES                                                    
         BE    JBE14                                                            
         CLC   =C'1C ',JOBPRM                                                   
         BE    JBE14                                                            
         CLC   =C'2C ',JOBPRM                                                   
         BE    JBE14                                                            
         CLC   =C'3C ',JOBPRM                                                   
         BE    JBE14                                                            
         CLC   =C'4C ',JOBPRM                                                   
         BE    JBE14                                                            
         B     ERROR                                                            
*                                  FOR NON-NEWS NO LINES,COLS,PRM               
JBE12    DS    0H                                                               
         LA    R2,JOBLINH                                                       
         LA    R3,USEERR                                                        
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
         LA    R2,JOBCOLH                                                       
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
         LA    R2,JOBPRMH                                                       
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
*                                  BFD                                          
JBE14    DS    0H                                                               
*                                  ALLOCATION                                   
JBE18    DS    0H                                                               
         LA    R2,JOBALOH                                                       
         LA    R3,INVERR                                                        
         CLI   5(R2),0                                                          
         BE    JBE18B                                                           
         CLC   SVPRD,=C'ZZZ'                                                    
         BNE   ERROR                                                            
         B     JBE18D                                                           
*                                                                               
JBE18B   DS    0H                                                               
*              USED TO REQUIRE ALLO FOR ZZZ                                     
         B     JBE18D                                                           
*                                  CHK NOW NO-OPED                              
*                                                                               
****     CLC   SVPRD,=C'ZZZ'                                                    
*****    BE    ERROR                                                            
*                                                                               
JBE18D   DS    0H                                                               
         OC    JOBALO,SPACES                                                    
         MVC   PJOBALO,JOBALO                                                   
*                                                                               
JBE20    DS    0H                                                               
*                                                                               
         LA    R2,JOBSTAH                                                       
         LA    R3,DATERR                                                        
         LA    R4,PJOBSTA                                                       
*                                                                               
JBE21    DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    JBE21B                                                           
         GOTO1 VDATVAL,DMCB,(0,8(R2)),WORK                                      
*                                                                               
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
*                                                                               
*        GOTO1 VDTCNV,DMCB,(0,WORK),(1,0(R4))                                   
         GOTO1 VDATCON,DMCB,(0,WORK),(3,0(R4))                                  
*                                                                               
JBE21B   DS    0H                                                               
         LA    R0,JOBENDH                                                       
         CR    R2,R0                                                            
         BE    JBE22                                                            
         LA    R2,JOBENDH                                                       
         LA    R4,PJOBEND                                                       
         B     JBE21                                                            
*                                                                               
JBE22    DS    0H                                                               
         OC    PJOBEND,PJOBEND                                                  
         BZ    JBE24                                                            
         LA    R3,ENDERR                                                        
         CLC   PJOBSTA,PJOBEND                                                  
         BH    ERROR                                                            
*                                                                               
JBE24    DS    0H                                                               
         OC    JOBSIG,SPACES                                                    
         MVC   PJOBSIG,JOBSIG                                                   
*                                                                               
         XC    PJOBPJOB,PJOBPJOB      CLEAR                                     
         CLI   JOBPJOBH+5,0            CHECK FOR INPUT                          
         BE    JBE24B                                                           
         MVC   PJOBPJOB,JOBPJOB        PRODUCTION JOB                           
         OC    PJOBPJOB,SPACES                                                  
*                                                                               
JBE24B   DS    0H                                                               
         MVC   PJOBBLCC,JOBBLCC        BILLING CONTACT                          
*                                                                               
         XC    PJOBFILT,PJOBFILT                                                
         LA    R2,JOBFILTH                                                      
         CLI   5(R2),0           CHK FOR INPUT                                  
         BE    JBE25                                                            
         LA    R3,INVERR                                                        
         CLI   5(R2),6                                                          
         BH    ERROR                                                            
         LA    R5,JOBFILT                                                       
         LA    R6,6            FOR BCT                                          
JBE24C   CLI   0(R5),C'A'                                                       
         BNL   JBE24D                                                           
         CLI   0(R5),0                                                          
         BE    JBE24D                                                           
         B     ERROR        SHOULD CATCH SPACES AND SPECIAL CHARS               
*                                                                               
JBE24D   LA    R5,1(R5)                                                         
         BCT   R6,JBE24C                                                        
         MVC   PJOBFILT,JOBFILT                                                 
*                                                                               
JBE25    DS    0H                                                               
         XC    JOBPRON,JOBPRON                                                  
         LA    R2,JOBPRODH                                                      
***                                                                             
***      REP NAME SEARCHING                                                     
***                                                                             
         SR    R2,RA                                                            
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         MVC   DSPARM(DSPARML),SPACES                                           
         MVC   DSMEDCOD,SVMED                                                   
         DROP  R3                                                               
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',(RA)),ACOMFACS,       X        
               ('DSPARML',WORK),(1,=CL8'REP'),0,RR=RELO                         
         LA    R2,JOBPRODH                                                      
*****                                                                           
         LA    R3,RECERR                                                        
         CLI   5(R2),0                                                          
         BE    JBE26                                                            
         CLI   5(R2),4                                                          
         BH    ERROR                                                            
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BNO   ERROR                                                            
         ZIC   RF,5(R2)            GET INPUT LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)         PACK INPUT                                   
         UNPK  8(4,R2),DUB         ADDS IN LEADING ZEROS                        
         FOUT  JOBPRODH            RE-DISPLAY                                   
*                                                                               
         MVC   PJOBPROD,JOBPROD                                                 
         MVC   WORKA(64),KEY                                                    
         XC    KEY+4(21),KEY+4                                                  
         MVI   KEY+3,X'11'                                                      
         MVC   KEY+4(4),8(R2)                                                   
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         MOVE  (IOAREA2,500),IOAREA     SAVE RECORD                             
         BAS   RE,GETREC                                                        
         MVC   JOBPRON,PREPNAME                                                 
*                                                                               
         MVC   KEY(64),WORKA                                                    
         CLI   JOBACT,C'C'                                                      
         BNE   *+8                                                              
         BAS   RE,GETREC           RESTORE FOR PUTREC                           
         MOVE  (IOAREA,500),IOAREA2                                             
*                                                                               
JBE26    DS    0H                                                               
         FOUT  JOBPRONH                                                         
*                                                                               
JBE30    DS    0H                                                               
         MVI   PJOBFSI,0                                                        
         LA    R2,JOBFSIH                                                       
         CLI   5(R2),0           CHK FOR INPUT                                  
         BE    JBE35                                                            
         LA    R3,INVERR                                                        
         CLI   5(R2),1                                                          
         BH    ERROR                                                            
         CLI   JOBFSI,C'N'        LEAVE N AS X'00'                              
         BE    JBE35                                                            
         CLI   JOBFSI,C'Y'                                                      
         BNE   ERROR                                                            
         MVC   PJOBFSI,JOBFSI                                                   
*                                                                               
JBE35    DS    0H                                                               
*                                                                               
*        BILLING REP                                                            
*                                                                               
         XC    JOBBRPN,JOBBRPN                                                  
         LA    R2,JOBBREPH                                                      
***                                                                             
***      REP NAME SEARCHING                                                     
***                                                                             
         SR    R2,RA                                                            
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         MVC   DSPARM(DSPARML),SPACES                                           
         MVC   DSMEDCOD,SVMED                                                   
         DROP  R3                                                               
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',(RA)),ACOMFACS,       X        
               ('DSPARML',WORK),(1,=CL8'REP'),0,RR=RELO                         
         LA    R2,JOBBREPH                                                      
*****                                                                           
         LA    R3,RECERR                                                        
         CLI   5(R2),0                                                          
         BE    JBE40                                                            
         CLI   5(R2),4                                                          
         BH    ERROR                                                            
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BNO   ERROR                                                            
         ZIC   RF,5(R2)            GET INPUT LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)         PACK INPUT                                   
         UNPK  8(4,R2),DUB         ADDS IN LEADING ZEROS                        
*                                                                               
         FOUT  JOBBREPH            RE-DISPLAY FIREL                             
*                                                                               
         MVC   PJOBBREP,JOBBREP                                                 
         MVC   WORKA(64),KEY                                                    
         XC    KEY+4(21),KEY+4                                                  
         MVI   KEY+3,X'11'                                                      
         MVC   KEY+4(4),8(R2)                                                   
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         MOVE  (IOAREA2,500),IOAREA     SAVE RECORD                             
         BAS   RE,GETREC                                                        
         MVC   JOBBRPN,PREPNAME                                                 
*                                                                               
         MVC   KEY(64),WORKA                                                    
         CLI   JOBACT,C'C'                                                      
         BNE   *+8                                                              
         BAS   RE,GETREC           RESTORE FOR PUTREC                           
         MOVE  (IOAREA,500),IOAREA2                                             
*                                                                               
JBE40    DS    0H                                                               
         FOUT  JOBBRPNH                                                         
*                                                                               
*        PUB LIST                                                               
*                                                                               
         XC    JOBPLSN,JOBPLSN                                                  
         LA    R2,JOBPLISH                                                      
         CLI   5(R2),0             ANYTHING ENTERED ?                           
         BE    JBE50X              NO - DONE                                    
         LA    R3,RECERR                                                        
*                                                                               
         MVC   PJOBPLIS,8(R2)                                                   
         OC    PJOBPLIS,SPACES                                                  
         MVC   WORKA(64),KEY                                                    
         XC    KEY+7(18),KEY+7                                                  
         MVI   KEY+3,X'17'         PUBLIST RECORD                               
         MVC   KEY+7(3),PJOBPLIS                                                
JBE50B   BAS   RE,HIGH                                                          
         CLC   KEY(10),KEYSAVE                                                  
         BE    JBE50G                                                           
         CLC   KEYSAVE+4(3),=C'ZZZ'   WAS I LOOKING FOR ALL CLIENTS ?           
         BE    ERROR                  YES - RECORD NOT FOUND                    
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+4(3),=C'ZZZ'                                                 
         B     JBE50B              TRY FOR "ZZZ" CLIENT CODE LIST REC           
JBE50G   DS    0H                                                               
         MOVE  (IOAREA2,500),IOAREA     SAVE RECORD                             
         BAS   RE,GETREC                                                        
         MVC   JOBPLSN(20),PJOBREC+40                                           
*            * (PUB LIST DESCRIPTION IS 40 BYTES PAST RECORD START) *           
*                                                                               
         MVC   KEY(64),WORKA                                                    
         CLI   JOBACT,C'C'                                                      
         BNE   *+8                                                              
         BAS   RE,GETREC           RESTORE FOR PUTREC                           
         MOVE  (IOAREA,500),IOAREA2                                             
*                                                                               
JBE50X   DS    0H                                                               
         FOUT  JOBPLSNH                                                         
*                                  SET ORIGIN CODE IN JOBREC                    
*                                  ON ADD AND CHANGE                            
         MVC   PJOBORIG,10(RA)                                                  
*                                                                               
*                                                                               
JBEX     DS    0H                                                               
         L     RE,SAVRE                                                         
         BR    RE                                                               
         SPACE 3                                                                
EJBMSG   DC    C' ENTER AD RECORD DATA'                                         
JRAMSG   DC    C' AD RECORD ADDED'                                              
JRCMSG   DC    C' AD RECORD CHANGED'                                            
JRDMSG   DC    C' AD RECORD DISPLAYED'                                          
DUPIDMSG DC    C' ** INVALID - Ad-ID ALREADY USED IN AD CODE:'                  
DUPIDMS2 DC    C' ** INVALID - Ad-ID ALREADY USED WITHOUT AD CODE'              
BOTHMSG  DC    C' ** INVALID - CANNOT ADD BOTH Ad-ID AND AD CODE'               
         EJECT                                                                  
*                                                                               
SVJOBSPC DS    CL17                SAVE JOB SPACE DESP BEFORE CHANGES           
*                                                                               
* INITIALISATION CODE                                                           
*                                                                               
INITL    LR    R4,RC               SET UP TO CLEAR WORK SPACE                   
         LR    R5,RD                                                            
         SR    R5,R4                                                            
         LR    R0,RE                                                            
         BAS   RE,CLEARWRK                                                      
         LM    R2,R4,0(R1)                                                      
         PACK  AGYNUM,0(1,R1)      AGENCY NUMBER                                
         LH    R5,0(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,FRSTFLD .        A(FIRST INPUT FIELD HEADER)                  
         LH    R5,2(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,LASTFLD .        A(LAST INPUT FIELD)                          
         MVC   NUMFLD,4(R2) .      NUMBER OF FIELDS                             
         ST    R3,VTWA .           A(TWA)                                       
         MVC   VDATAMGR(44),0(R4)  FACILITY LIST                                
         LR    RA,R3                                                            
         MVC   TERMNAL,0(RA) .     TERMINAL NUMBER                              
         MVC   AGYALPHA,14(RA)     ALPHA AGENCY CODE                            
         LA    R3,64(R3)                                                        
         ST    R3,ERRAREA          PRESET ERRAREA TO A(FIRST HEADER)            
         MVI   DMINBTS,X'C0'       PRESET DATAMGR CONTROL BITS                  
         MVI   DMOUTBTS,X'FD'      PRESET DATAMGR ERROR CHECK BITS              
         LA    R3,IOAREA                                                        
         ST    R3,AREC                                                          
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
CLEARWRK LTR   R5,R5               CLEAR STORAGE TO ZEROS                       
         BCR   8,RE                                                             
         CH    R5,=H'250'                                                       
         BNH   CLEAREST                                                         
         XC    0(250,R4),0(R4)                                                  
         LA    R4,250(R4)                                                       
         SH    R5,=H'250'                                                       
         B     CLEARWRK                                                         
         SPACE 2                                                                
CLEAREST BCTR  R5,R0                                                            
         EX    R5,VARCLEAR                                                      
         BR    RE                                                               
         SPACE 2                                                                
VARCLEAR XC    0(0,R4),0(R4)                                                    
         EJECT                                                                  
*                  FARMABLE CODE                                                
         SPACE 3                                                                
ANY      CLI   5(R2),0                                                          
         BNE   ANY2                                                             
         LA    R3,1                                                             
         B     ERROR                                                            
         SPACE 2                                                                
ANY2     TM    4(R2),X'10' .       IS IT VALID NUMERIC                          
         BCR   8,RE .              IF APPLICABLE                                
         LA    R3,3                                                             
         B     ERROR                                                            
         SPACE 2                                                                
PACK     SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         TM    4(R2),X'08'                                                      
         BCR   8,RE                        OR NON NUMERIC                       
         CHI   R1,10              WAS A BUG IF LENGTH IS BIGGER                 
         BL    PACK01             THEN 10                                       
         BR    RE                                                               
PACK01   BCTR  R1,R0                                                            
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
         BR    RE                                                               
         SPACE 2                                                                
VARPACK  PACK  DUB,8(0,R2)                                                      
         SPACE 2                                                                
MOVE     MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         BCTR  R1,R0                                                            
         EX    R1,VARMOVE                                                       
         BR    RE                                                               
         SPACE 2                                                                
VARMOVE  MVC   WORK(0),8(R2)                                                    
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (DIRECTORY)                  
         SPACE 3                                                                
READ     MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
         SPACE 2                                                                
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
         SPACE 2                                                                
ADD      MVC   COMMAND,=C'DMADD '                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
WRITE    MVC   COMMAND,=C'DMWRT '                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
DIRCTRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (FILE)                       
         SPACE 3                                                                
*                                                                               
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
ADDREC   MVC   COMMAND,=C'ADDREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
FILE     NTR                                                                    
         LA    R2,KEY+27                                                        
         CLC   COMMAND(5),=C'DMDEL'                                             
         BE    *+12                                                             
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE',            X        
               (R2),AREC,(TERMNAL,DMWORK)                                       
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (PUBDIR)                     
         SPACE 3                                                                
READPUB  MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     PUBDIRY                                                          
         SPACE 2                                                                
SEQPUB   MVC   COMMAND,=C'DMRSEQ'                                               
         B     PUBDIRY                                                          
         SPACE 2                                                                
HIGHPUB  MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     PUBDIRY                                                          
         SPACE 2                                                                
WRITEPUB MVC   COMMAND,=C'DMWRT'                                                
         B     PUBDIRY                                                          
         SPACE 2                                                                
ADDPUBD  MVC   COMMAND,=C'DMADD '                                               
         B     PUBDIRY                                                          
         SPACE 2                                                                
PUBDIRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (PUBFILE)                    
         SPACE 3                                                                
GETPUB   MVC   COMMAND,=C'GETREC'                                               
         B     PUBFILE                                                          
         SPACE 2                                                                
PUTPUB   MVC   COMMAND,=C'PUTREC'                                               
         B     PUBFILE                                                          
         SPACE 2                                                                
ADDPUB   MVC   COMMAND,=C'ADDREC'                                               
         B     PUBFILE                                                          
         SPACE 2                                                                
PUBFILE  NTR                                                                    
         LA    R2,KEY+27                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBFILE',            X        
               (R2),APUBIO,(TERMNAL,DMWORK)                                     
         B     DMCHECK                                                          
         EJECT                                                                  
*                  DATA MANAGER ERRORS AND EXIT                                 
         SPACE 3                                                                
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         XIT                                                                    
         SPACE 2                                                                
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3 .             LET GETMSG SORT IT OUT                       
         B     ERROR                                                            
         EJECT                                                                  
*                  EXITS FROM PROGRAM                                           
         SPACE 3                                                                
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
         SPACE 2                                                                
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
         STCM  R2,15,SVERRCUR                                                   
EXIT     OI    6(R2),OI1C .        INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKSPDESP NTR1  BASE=*,LABEL=*      WORK HAS CURRENT SPACE DESP VALUE            
*                                                                               
         CLI   JOBSPC,C'='         BROWSING?                                    
         BNE   CKSPD30                                                          
*                                                                               
         GOTOR VPBROWSE,DMCB,ACOMFACS,SYSRD,(R2),                      +        
               0,(SVMED,C' STD'),0                                              
*                                                                               
         DC    H'0'                BROWSE KNOWS WHERE TO GET BACK               
*                                                                               
CKSPD30  MVC   CKSPKEY(L'KEY),KEY                                               
         MVC   CKSPKEY+L'KEY(L'KEYSAVE),KEYSAVE                                 
*                                                                               
         XC    CKSPWORK,CKSPWORK                                                
         MVC   CKSPWORK+30(4),=C'P0BY'                                          
         MVC   CKSPWORK+34(2),AGYALPHA                                          
         MVC   CKSPWORK+36(1),SVMED                                             
         MVC   CKSPWORK+37(3),SVCLT                                             
         CLI   SVCLTOFF,C' '       OFFICE CODE PRESENT?                         
         BNH   *+14                                                             
         MVI   CKSPWORK+40,C'*'                                                 
         MVC   CKSPWORK+41(1),SVCLTOFF                                          
*                                                                               
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         GOTO1 CGETPROF,DMCB,(0,CKSPWORK+30),CKSPWORK,VDATAMGR                  
         DROP  R4                                                               
*                                                                               
         CLI   CKSPWORK+9,C'Y'     PROFILE ALLOWS SPACE DESP LOOK UP?           
         BNE   CKSPDX2             NO NEED TO LOOK UP SPC DESP RECORD           
*                                                                               
* SEE IF SPACE DESP ENTERED IS SAME AS IN RECORD NOW                            
*                                                                               
         CLC   JOBSPC,WORK         SAME AS SPACE DESP?                          
         BE    CKSPDX2             NO NEED TO LOOK UP SPC DESP RECORD           
*                                                                               
         CLC   JOBSPC,=17C' '      NO INPUT?                                    
         BE    CKSPDX2             NO NEED TO LOOK UP SPC DESP RECORD           
*                                                                               
         XC    KEY,KEY             BUILD KEY TO READ SPC DESP RECORD            
         MVC   KEY+00(02),AGYALPHA                                              
         MVC   KEY+02(01),SVMED                                                 
         MVI   KEY+03,X'5A'        STANDARD SPACE DESCRIPTION REC CODE          
         MVC   KEY+04(17),JOBSPC                                                
*                                                                               
         MVC   KEYSAVE,KEY         FOR COMPARISON LATER                         
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KEY,KEY                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(21),KEYSAVE     INPUT MATCHED THAT OF RECORD?                
         BE    CKSPDX2             YES, VALIDATED AND DONE                      
         LA    R3,SPDSPERR                                                      
*                                                                               
CKSPDX1  DS    0H                                                               
         MVC   KEY,CKSPKEY                                                      
         MVC   KEYSAVE,CKSPKEY+L'KEY                                            
         XIT1  REGS=(R2,R3)        ERROR MSG AND CURSOR POSITION                
*                                                                               
CKSPDX2  DS    0H                                                               
         MVC   KEY,CKSPKEY                                                      
         MVC   KEYSAVE,CKSPKEY+L'KEY                                            
         XIT1                      ALL REGISTERS RESTORED                       
*                                                                               
CKSPWORK DS    CL250               WORKING STORAGE AREA                         
CKSPKEY  DS    CL100               FOR RESTORING KEYS                           
RELOSPD  DS    A                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                EDIT AD ID FIELD                                     *         
*    WORK HAS THE 12-BYTE AD ID FROM THE CURRENT SCREEN               *         
*    FIRST 4 BYTES MUST BE ALPHA, NEXT 8 CAN BE ALPHANUMERIC          *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
EDTADID  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,WORK             POINT R4 TO 12-BYTE AD ID ENTRY              
         LA    R5,12               LOOP COUNTER                                 
EDTAD10  DS    0H                                                               
         CLI   0(R4),C'A'                                                       
         BL    EDTADER             NO GOOD                                      
         CLI   0(R4),C'I'                                                       
         BNH   EDTAD20             CHARACTER IS OK                              
         CLI   0(R4),C'J'                                                       
         BL    EDTADER             NO GOOD                                      
         CLI   0(R4),C'R'                                                       
         BNH   EDTAD20             CHARACTER IS OK                              
         CLI   0(R4),C'S'                                                       
         BL    EDTADER             NO GOOD                                      
         CLI   0(R4),C'Z'                                                       
         BNH   EDTAD20             CHARACTER IS OK                              
         CHI   R5,8                FIRST 4 CHARACTERS MUST BE ALPHA             
         BH    EDTADER             NO GOOD                                      
         CLI   0(R4),C'0'                                                       
         BL    EDTADER             NO GOOD                                      
         CLI   0(R4),C'9'                                                       
         BH    EDTADER             NO GOOD                                      
EDTAD20  DS    0H                                                               
         LA    R4,1(R4)            BUMP TO NEXT CHARACTER                       
         BCT   R5,EDTAD10                                                       
         B     EDTADOK             NO EDIT ERRORS                               
*                                                                               
EDTADER  DS    0H                  ERROR EXIT                                   
         CR    RB,RB               SET CC EQUAL                                 
         B     EDTADXIT                                                         
*                                                                               
EDTADOK  DS    0H                                                               
         LTR   RB,RB               SET CC NOT EQUAL                             
         B     EDTADXIT                                                         
*                                                                               
EDTADXIT DS    0H                                                               
         XIT1  REGS=(R3)           "MAY" NEED R3 FOR ERROR NO                   
*                                                                               
         DROP RB                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*        FIND AND RESERVE THE NEXT AVAILABLE AD-ID ONLY RECORD        *         
*          UNIQUE SEQUENCE NUMBER                                     *         
*                                                                     *         
*        START NUMBERING AT 1,000 FOR EACH MED/CLT/PRD                *         
*                                                                     *         
*        ROUTINE READS FILE FOR PASSIVE POINTER THAT HAS              *         
*           SEQUENCE NUMBERS IN 2'S COMPLEMENT. READS LOWEST NUMBER   *         
*           (REALLY HIGHEST) FOR UPDATE AND THEN ADDS POINTER FOR     *         
*           NEXT NUMBER. THIS RESERVES NEXT NUMBER FOR THIS CALL TO   *         
*           THE SUBROUTINE.                                           *         
*           IF THIS RESULTS IN A DUPLICATE KEY THE PROCESS IS         *         
*           REPEATED.                                                 *         
*                                                                     *         
*EXIT    QSQN#  =  FOUND NEW SEQUENCE NUMBER                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VNXTSQN# NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVKEY,KEY           SAVE CURRENT KEY                             
*                                                                               
         LA    R4,KEY              ESTABLISH KEY AS CUSTOM COLUMN               
         USING PADNRECD,R4           SEQUENCE# PASSIVE                          
*                                                                               
NXTSQNLP DS    0H                                                               
*                                                                               
         XC    KEY,KEY             INIT KEY                                     
*                                                                               
         MVC   PADNKAGY,AGYALPHA   SET AGENCY                                   
         MVC   PADNKMED,SVMED      SET MEDIA                                    
         MVI   PADNKRCD,PADNKRCQ   SET RECORD CODE                              
         MVC   PADNKCLT,SVCLT      SET CLIENT                                   
         MVC   PADNKPRD,SVPRD      SET CLIENT                                   
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                READ FOR FIRST PASSIVE ON DIRECTORY          
*                                                                               
         NI    DMINBTS,X'FF'-X'08'  RESET                                       
*                                                                               
         CLC   PADNKEY(PADNKJOB-PADNKEY),KEYSAVE   SKIP IF FOUND                
         BE    NXTSQN1                                                          
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE STARTING KEY                         
*                                                                               
         MVI   PADNKJOB,X'FF'                                                   
         MVC   PADNKJOB+1(2),=X'03E8'   START SQN# AT 1,000                     
         XC    PADNKJOB+1(2),=X'FFFF'   2'S COMPLEMENT                          
*                                                                               
         B     NXTSQN2                                                          
*                                                                               
*        READ RECORD AND RESERVE NEXT SQN#                                      
*                                                                               
NXTSQN1  DS    0H                                                               
*                                                                               
         OI    DMINBTS,X'80'       READ FOR UPDATE                              
*                                                                               
         GOTO1 READ                READ FOR UPDATE TO LOCK BLK OF REC'D         
*                                                                               
         NI    DMINBTS,X'FF'-X'80'   RESET                                      
*                                                                               
         ZICM  RF,PADNKJOB+1,2       MOVE SEQUENCE NUMBER TO WORK               
*                                                                               
         AHI   RF,-1               DECREMENT 2'S COMPLEMENT = NXT SQN#          
*                                                                               
         STH   RF,HALF                                                          
         MVC   PADNKJOB+1(2),HALF     PUT NEW COMPLEMENT IN KEY                 
*                                                                               
NXTSQN2  DS    0H                                                               
*                                                                               
         GOTO1 ADD                 ADD TO FILE                                  
*                                                                               
         CLI   DMCB+8,0            DONE IF NO DMGR ERRORS                       
         BE    NXTSQNDN                                                         
*                                                                               
         TM    DMCB+8,X'20'        OKAY IF DUPE KEY FOUND                       
         BO    *+6                                                              
         DC    H'0'                DUPE RECORD ONLY ERROR ALLOWED               
*                                                                               
NXTSQNCN DS    0H                                                               
*                                                                               
         B     NXTSQNLP            REPEAT SEARCH FOR NEXT #                     
*                                                                               
NXTSQNDN DS    0H                                                               
*                                                                               
         MVC   QSQN#,PADNKJOB+1    SAVE NEXT SERIAL NUMBER                      
*                                                                               
         MVC   KEY,SVKEY           RESTORE CURRENT KEY                          
         GOTO1 HIGH                RESTORE FILE POINTERS                        
*                                                                               
VNXTSQNX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
       ++INCLUDE PPJOBWRK                                                       
*                                                                               
       ++INCLUDE PPSRCHPARM                                                     
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
*                                                                               
*PADIRECD (DSECT)                                                               
       ++INCLUDE PPGENPADID                                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'088PPJOB01   07/07/14'                                      
         END                                                                    
