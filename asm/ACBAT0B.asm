*          DATA SET ACBAT0B    AT LEVEL 020 AS OF 05/01/02                      
*PHASE T61B0BA                                                                  
         TITLE 'ANALYSED TRANSFER'                                              
T61B0B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,**BAT0B,RR=R5,CLEAR=YES                             
         USING TWAD,RA                                                          
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING PROGD,RC                                                         
         ST    R5,PRELOC                                                        
         EJECT                                                                  
*              VALIDATE INPUT ACCOUNTS & SAVE NAMES AND NUMBERS                 
         SPACE 2                                                                
         SR    R6,R6               NO PROFILES FOR NOW                          
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVI   FLAGDRCR,C' '       SOURCE OF FLAGGING (IF ANY)                  
         XC    SICONTRA,SICONTRA                                                
         LA    R5,COMPEL                                                        
         USING ACCOMPD,R5                                                       
         SR    R3,R3                                                            
         MVI   OFFSW,C'N'          TEST FOR MANDATORY OFFICE/UNIT               
         TM    COMPSTAT,X'20'                                                   
         BZ    AT00                                                             
         MVI   OFFSW,C'Y'                                                       
         LA    R2,ANTUNITH                                                      
         BAS   RE,ANY                                                           
AT00     DS    0H                                                               
*&&UK                                                                           
         CLI   TWAACCS,C'*'        IF OFFICE LOGON TEST IF                      
         BNE   AT02                                                             
         CLC   ANTUNIT,TWAACCS+1   INPUT IS FOR CORRECT OFFICE                  
         BE    AT02                                                             
         MVI   ERRNUM,SECLOCK                                                   
         B     ERROR                                                            
*&&                                                                             
AT02     LA    R2,ANTDRH                                                        
         BAS   RE,ANY              DEBIT A/C                                    
         MVI   ERRNUM,17                                                        
         IC    R3,ANTDRH+5                                                      
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+1(0),ANTDR                                                   
         LA    RF,DLIST                                                         
AT08     CLI   0(RF),X'FF'         TEST DEBIT EXCLUSIONS                        
         BE    AT1                                                              
         CLC   ANTDR(2),0(RF)                                                   
         BE    ERROR                                                            
         LA    RF,2(RF)                                                         
         B     AT08                                                             
AT1      DS    0H                                                               
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVI   STFSW,C'N'                                                       
         MVI   DEPSW,C'N'                                                       
         TM    ACCTSTAT,X'40'      GENERATE PERSONAL EXPENSE?                   
         BZ    AT1A                                                             
         MVI   STFSW,C'Y'                                                       
         MVI   FLAGDRCR,C'D'                                                    
AT1A     TM    ACCTSTAT,X'08'      DEPARTMENTAL EXPENSE                         
         BZ    AT1C                                                             
         MVI   DEPSW,C'Y'                                                       
         MVI   FLAGDRCR,C'D'                                                    
AT1C     MVC   DRNUM,ACCTNUM                                                    
         MVC   DRNAME,ACCTNAME                                                  
         XC    STAFFNUM,STAFFNUM                                                
         MVC   COSTBYTE,ACCTCOST                                                
         CLI   COSTBYTE,C' '                                                    
         BE    *+8                                                              
         MVI   FLAGDRCR,C'D'                                                    
         TM    ANTDRH+4,X'20'                                                   
         BO    AT2                                                              
         MVC   ANTDRN,DRNAME                                                    
         FOUT  ANTDRNH                                                          
         OI    ANTDRH+4,X'20'                                                   
         SPACE 2                                                                
AT2      LA    R2,ANTCRH                                                        
         BAS   RE,ANY              CREDIT A/C                                   
         MVC   KEY+1(14),SPACES                                                 
         IC    R3,ANTCRH+5                                                      
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+1(0),ANTCR                                                   
         LA    RF,CLIST                                                         
AT2AA    CLI   0(RF),X'FF'         TEST CREDIT EXCLUSIONS                       
         BE    AT2AC                                                            
         CLC   ANTCR(2),0(RF)                                                   
         BE    ERROR                                                            
         LA    RF,2(RF)                                                         
         B     AT2AA                                                            
AT2AC    DS    0H                                                               
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         CLI   FLAGDRCR,C' '       ANY FLAGS ON DR ACCOUNT                      
         BNE   AT2A                                                             
         CLI   ACCTCOST,X'41'      IF NOT CAN PICK UP COSTING FLAG              
         BL    AT2C                FROM CREDIT ACCOUNT                          
         MVC   COSTBYTE,ACCTCOST                                                
         MVI   FLAGDRCR,C'C'                                                    
         B     AT2C                                                             
AT2A     CLI   ACCTCOST,C' '       FLAG ON DR ACCT - NONE ALLOWED ON CR         
         BNE   ERROR                                                            
AT2C     MVC   CRNUM,ACCTNUM                                                    
         MVC   CRNAME,ACCTNAME                                                  
         TM    ANTCRH+4,X'20'                                                   
         BO    AT4                                                              
         MVC   ANTCRN,ACCTNAME                                                  
         FOUT  ANTCRNH                                                          
         OI    ANTCRH+4,X'20'                                                   
         SPACE 2                                                                
AT4      MVC   KEY(15),DRNUM       BACK TO DEBIT ACCOUNT                        
         LA    R2,ANTDRH                                                        
         CLI   DEPSW,C'Y'                                                       
         BNE   AT4C                                                             
         MVC   KEY+1(2),=C'28'     HARD DEPT SUSP                               
*&&UK                                                                           
         CLI   COMPANY,C'1'        JWT HARD CODE                                
         BNE   *+10                                                             
         MVC   KEY+6(9),SPACES                                                  
*&&                                                                             
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   CRDSNUM,ACCTNUM                                                  
         MVC   CRDSNAME,ACCTNAME                                                
         SPACE 2                                                                
AT4C     DS    0H                                                               
         CLI   COSTBYTE,C' '       MAKING COSTING POSTINGS                      
         BE    AT10                                                             
         CLI   FLAGDRCR,C'D'       IF DOING POSTING FROM CR ACCOUNT             
         BNE   AT5C                RULES GO TO READ INCOME ACCOUNT              
         MVC   KEY+1(2),=C'1P'                                                  
         MVC   KEY+3(12),SPACES                                                 
         LA    RF,KEY+3                                                         
         CLI   OFFSW,C'N'                                                       
         BE    AT4D                                                             
*&&UK*&& B     AT4D                ** NOV 84 **                                 
         MVC   KEY+3(1),ANTUNIT                                                 
         LA    RF,1(RF)                                                         
AT4D     MVC   0(2,RF),=C'99'      FOR NO DEPT-BUT COSTING REQUIRED             
         LA    RF,2(RF)                                                         
         CLI   DEPSW,C'Y'                                                       
         BNE   AT4E                                                             
         LA    R2,ANTDEPH                                                       
         BAS   RE,ANY                                                           
         MVC   KEY+3(12),SPACES                                                 
         LA    RE,KEY+3                                                         
*&&US                                                                           
         CLI   OFFSW,C'N'                                                       
         BE    *+14                                                             
         MVC   KEY+3(1),ANTUNIT                                                 
         LA    RE,1(RE)                                                         
*&&                                                                             
         MVI   ERRNUM,17                                                        
         ZIC   RF,ANTDEPH+5                                                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ANTDEP                                                   
         LA    RF,1(RE,RF)                                                      
AT4E     MVC   0(1,RF),COSTBYTE                                                 
AT5      BAS   RE,GETACC                                                        
         TM    ACCTSTAT,X'80'                                                   
         BZ    BADACC                                                           
         MVC   CRCNUM,ACCTNUM                                                   
         MVC   CRCNAME,ACCTNAME                                                 
         SPACE 1                                                                
         MVC   KEY+2(13),SPACES                                                 
         MVI   KEY+2,C'3'                                                       
         MVC   KEY+3(1),COSTBYTE                                                
         BAS   RE,GETACC                                                        
         MVC   CR13NUM,ACCTNUM                                                  
         MVC   CR13NAME,ACCTNAME                                                
         B     AT5X                                                             
         SPACE 2                                                                
AT5C     MVC   KEY+1(14),SPACES    READ COSTING INCOME ACCOUNT                  
         MVC   KEY+1(2),=C'12'                                                  
         MVC   KEY+3(1),COSTBYTE                                                
         BAS   RE,GETACC                                                        
         TM    ACCTSTAT,X'80'                                                   
         BZ    BADACC                                                           
         MVC   CINUM,ACCTNUM                                                    
         MVC   CINAME,ACCTNAME                                                  
         SPACE 2                                                                
AT5X     LA    R2,ANTCLIH                                                       
         BAS   RE,ANY                                                           
         LA    R6,CLIPROF                                                       
         MVC   KEY+1(14),SPACES                                                 
         MVC   KEY+1(2),ACMPJOB    PRODUCTION LEDGER                            
         IC    R3,ANTCLIH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),ANTCLI                                                  
         BAS   RE,GETACC                                                        
         TM    ANTCLIH+4,X'20'                                                  
         BO    AT6                                                              
         NI    ANTPROH+4,X'DF'                                                  
         XC    PRODPROF,PRODPROF                                                
         XC    JOBPROF,JOBPROF                                                  
         MVC   ANTCLIN,ACCTNAME                                                 
         FOUT  ANTCLINH                                                         
         OI    ANTCLIH+4,X'20'                                                  
         SPACE 1                                                                
AT6      LA    R2,ANTPROH                                                       
         CLI   ANTPROH+5,0         ANY COSTING TO PRODUCT?                      
         BE    AT8                 NO-BRANCH                                    
         LA    R4,KEY+3                                                         
         IC    R3,CLILNGTH         LEVA LENGTH                                  
         AR    R4,R3                                                            
         IC    R3,ANTPROH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),ANTPRO      MAKE KEY= CULCLIPRO                          
         TM    ANTPROH+4,X'20'                                                  
         BO    *+10                                                             
         XC    PRODPROF,PRODPROF                                                
         LA    R6,PRODPROF                                                      
         BAS   RE,GETACC                                                        
         TM    ANTPROH+4,X'20'                                                  
         BO    AT8                                                              
         MVC   ANTPRON,ACCTNAME                                                 
         FOUT  ANTPRONH                                                         
         OI    ANTPROH+4,X'20'                                                  
         SPACE 1                                                                
AT8      CLC   CRNUM+1(2),=C'SI'   SAVE CLI/PROD IF POSTING TO SI               
         BNE   AT9                                                              
         MVC   SICONTRA,KEY                                                     
         MVC   SICONTRN,ACCTNAME                                                
AT9      BAS   RE,PROFMERG                                                      
         LA    R4,PROFILE                                                       
         USING ACPROFD,R4                                                       
         MVC   COSTNUM,ACPRCOST                                                 
         MVC   KEY(15),ACPRCOST                                                 
         BAS   RE,GETACC                                                        
         LA    R2,ANTCLIH                                                       
         MVI   ERRNUM,18                                                        
         TM    ACCTSTAT,X'80'                                                   
         BZ    BADACC                                                           
         TM    ACCTSTAT,ACSLOCK                                                 
         BO    BADACC                                                           
         MVC   COSTNAME,ACCTNAME                                                
         DROP  R4                                                               
         SPACE 2                                                                
AT10     MVC   KEY+1(14),SPACES                                                 
         MVC   KEY+1(2),=C'2D'                                                  
         CLI   DEPSW,C'Y'                                                       
         BNE   AT12                                                             
         LA    R2,ANTDEPH          DEPARTMENT                                   
         BAS   RE,ANY                                                           
         SR    R6,R6               NO MORE PROFILES                             
         LA    RF,KEY+3                                                         
         CLI   ANTUNITH+5,0                                                     
         BE    AT11                                                             
         MVC   KEY+3(1),ANTUNIT                                                 
         LA    RF,KEY+4                                                         
AT11     DS    0H                                                               
         IC    R3,ANTDEPH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),ANTDEP      DEPT OR UNIT/DEPT                            
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   DEPNUM,ACCTNUM                                                   
         MVC   DEPNAME,ACCTNAME                                                 
         TM    ANTDEPH+4,X'20'                                                  
         BO    AT12                                                             
         MVC   ANTDEPN,ACCTNAME                                                 
         FOUT  ANTDEPNH                                                         
         OI    ANTDEPH+4,X'20'                                                  
         SPACE 3                                                                
AT12     LA    R2,ANTSTFH                                                       
         CLI   STFSW,C'Y'          PERSONNEL POSTING                            
         BNE   AT14                NO - BRANCH                                  
         BAS   RE,ANY              STAFF NO.                                    
         MVC   KEY+2(13),SPACES                                                 
         MVI   KEY+2,C'P'                                                       
         IC    R3,ANTSTFH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),ANTSTF                                                  
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   STAFFNUM,ACCTNUM                                                 
         MVC   STAFFNAM,ACCTNAME                                                
         TM    ANTSTFH+4,X'20'                                                  
         BO    AT13                                                             
         MVC   ANTSTFN,ACCTNAME                                                 
         FOUT  ANTSTFNH                                                         
         OI    ANTSTFH+4,X'20'                                                  
         SPACE 2                                                                
AT13     LA    R2,ANTCLIH                                                       
         MVI   KEY+2,C'9'          CLIENT IN 29                                 
         MVC   KEY+3(12),=CL12'999'                                             
*&&UK                                                                           
         CLI   KEY,C'G'            * HARD CODE FOR LINTAS *                     
         BNE   *+8                                                              
         MVI   KEY+6,C'0'                                                       
*&&                                                                             
         CLI   COSTBYTE,C' '       DUMMY OR REAL IF ONE DEMANDED                
         BE    *+10                                                             
         MVC   KEY+3(12),COSTNUM+3                                              
         BAS   RE,GETACC                                                        
         MVI   ERRNUM,18                                                        
         TM    ACCTSTAT,X'80'                                                   
         BO    AT13A                                                            
         CLI   COSTBYTE,C' '       IF '999' IS NOT LOW LEVEL                    
         BNE   ERROR                                                            
         MVC   KEY+6(2),=C'00'     TRY FOR '99900'                              
         BAS   RE,GETACC                                                        
         TM    ACCTSTAT,X'80'                                                   
         BZ    ERROR                                                            
AT13A    MVC   CRPSNUM,ACCTNUM                                                  
         MVC   CRPSNAME,ACCTNAME                                                
         EJECT                                                                  
*              BUILD 64 ELEMENT                                                 
         SPACE 2                                                                
AT14     LA    R8,IOAREA+2                                                      
         USING DLDESCD,R8                                                       
         MVI   DLDSEL,X'64'                                                     
         LA    R2,ANTDOCH                                                       
         BAS   RE,ANY                                                           
         MVC   DLDSREF,SPACES                                                   
         IC    R3,ANTDOCH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   DLDSREF(0),ANTDOC                                                
         SPACE 1                                                                
         LA    R2,ANTDATH                                                       
         MVI   ERRNUM,13                                                        
         CLI   ANTDATH+5,0                                                      
         BNE   *+12                                                             
         BAS   RE,GETODAY                                                       
         B     AT16                                                             
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         SPACE 1                                                                
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
AT16     GOTO1 DATCON,DMCB,(0,WORK),(1,DLDSDATE)                                
         GOTO1 DATECHK,DMCB,DLDSDATE                                            
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
*&&UK*&& OI    DLDSSTAT,X'08'      AUTHORISE                                    
         LA    R2,ANTNARH                                                       
         LA    R3,DLDSNARR                                                      
         XC    DLDSNARR,DLDSNARR                                                
         BAS   RE,NARRSCAN                                                      
         SR    R3,R3                                                            
         LA    R5,DLDSNARR                                                      
         SR    R5,R8               LENGTH OF ELEMENT-NARRATIVE                  
         AR    R5,R6                                                            
         STH   R5,HALF                                                          
         MVC   DLDSLEN,HALF+1                                                   
         EJECT                                                                  
*              BUILD 68 ELEMENTS                                                
         SPACE 2                                                                
         AR    R8,R5               BUMP TO NEXT ELEMENT                         
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,X'69'        DEBIT DR ACCOUNT                             
         MVI   DLPSLEN,X'71'                                                    
         MVC   DLPSDBAC,DRNUM                                                   
         MVC   DLPSDBNM,DRNAME                                                  
         MVC   DLPSCRAC,CRNUM                                                   
         MVC   DLPSCRNM,CRNAME                                                  
         CLC   DRNUM+1(2),=C'SR'   IF DR IS TO SR THEN USE                      
         BNE   AT16A               A PIECE OF CREDIT ACCOUNT NAME               
         MVC   DLPSCRNM,SPACES     AS BILLING SOURCE                            
         MVC   DLPSCRAC(3),SPACES                                               
         LA    R2,DLPSCRAC+3                                                    
         GOTO1 CHOPPER,DMCB,(36,CRNAME),(12,(R2)),(0,1)                         
AT16A    MVI   DLPSTYPE,0                                                       
         MVC   DLPSANAL,SPACES                                                  
         CLI   ANTUNITH+5,0                                                     
         BE    *+10                                                             
         MVC   DLPSANAL(1),ANTUNIT                                              
         SPACE 1                                                                
         IC    R3,ANTAMTH+5                                                     
         LA    R2,ANTAMTH                                                       
         MVI   ERRNUM,25                                                        
         GOTO1 AMTVAL,DMCB,ANTAMT,(R3)                                          
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         L     RF,DMCB+4                                                        
         LA    RF,0(RF)                                                         
         ZAP   DLPSAMNT,0(8,RF)                                                 
         ZAP   TRANSAMT,0(8,RF)    SAVE AMOUNT                                  
         IC    R3,DLPSLEN                                                       
         LR    RF,R8                                                            
         AR    R8,R3                                                            
         LR    RE,R3                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(RF)       DUPLICATE DR ELEMENT                         
         MVI   DLPSEL,X'6A'        THEN MAKE IT A CREDIT                        
         MVC   DLPSCRAC,CRNUM                                                   
         MVC   DLPSCRNM,CRNAME                                                  
         CLC   CRNUM+1(2),=C'SI'   AND IF CR TO SI                              
         BNE   AT16C                                                            
         OC    SICONTRA,SICONTRA   SEE IF WE CAN IMPROVE THE CONTRA             
         BZ    AT16C                                                            
         MVC   DLPSDBAC,SICONTRA   TO CLI/PROD                                  
         MVC   DLPSDBNM,SICONTRN                                                
         EJECT                                                                  
AT16C    CLI   STFSW,C'Y'                                                       
         BNE   AT17                                                             
         LR    RF,R8                                                            
         IC    R3,DLPSLEN                                                       
         AR    R8,R3                                                            
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(RF)       SECOND ELEMENT                               
         LA    R3,1(R3)                                                         
         MVI   DLPSEL,X'69'        DEBIT STAFF                                  
         MVC   DLPSDBAC,STAFFNUM                                                
         MVC   DLPSDBNM,STAFFNAM                                                
         MVC   DLPSCRNM,CRPSNAME                                                
         OI    DLPSTYPE,X'80'      SUBSIDIARY FROM HERE                         
         MVI   DLPSCRAC,C'*'       CONTRA IS *EXPENSE-CLIENT                    
         SR    R1,R1                                                            
         IC    R1,ANTDRH+5                                                      
         SH    R1,=H'3'                                                         
*&&UK                                                                           
         CLI   COMPANY,C'1'        JWT HARD CODE                                
         BNE   *+8                                                              
         LA    R1,2                                                             
*&&                                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLPSCRAC+1(0),ANTDR+2                                            
         CLI   TENO,X'F0'          ARE WE CHOPPING DOWN SE CODE                 
         BL    AT16E                                                            
         PACK  DUB,TENO                                                         
         CVB   R5,DUB                                                           
         MVC   DLPSCRAC+1(14),SPACES                                            
         LA    R2,ANTDRH                                                        
         MVI   ERRNUM,2                                                         
         LA    RF,1(R1)                                                         
         SR    RF,R5               INPUT MUST BE AT LEAST AS LONG               
         BM    ERROR               AS 'TENO' VALUE                              
         LA    RE,ANTDR+2(RF)                                                   
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   DLPSCRAC+1(0),0(RE)                                              
         LR    R1,R5                                                            
AT16E    STC   R1,BYTE                                                          
         LA    RF,DLPSCRAC+2(R1)                                                
         MVI   0(RF),C'-'                                                       
         LA    R1,DLPSCRAC+14                                                   
         SR    R1,RF                                                            
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),CRPSNUM+3                                                
         SPACE 1                                                                
         LR    RF,R8                                                            
         AR    R8,R3                                                            
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(RF)                                                    
         LA    R3,1(R3)                                                         
         MVI   DLPSEL,X'6A'        CREDIT CLIENT                                
         MVC   DLPSDBAC,DLPSCRAC                                                
         MVC   DLPSCRAC,CRPSNUM                                                 
         IC    R1,BYTE                                                          
         LA    RF,DLPSDBAC+2(R1)                                                
         LA    R1,DLPSDBAC+14                                                   
         SR    R1,RF                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),STAFFNUM+3  CONTRA IS *EXPENSE-STAFF                     
         SPACE 2                                                                
AT17     DS    0H                                                               
         CLI   DEPSW,C'Y'                                                       
         BNE   AT17A                                                            
         IC    R3,DLPSLEN                                                       
         LR    RF,R8                                                            
         AR    R8,R3                                                            
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(RF)       THIRD ELEMENT                                
         LA    R3,1(R3)                                                         
         MVC   DLPSDBAC,DEPNUM                                                  
         MVC   DLPSDBNM,DEPNAME                                                 
         MVC   DLPSCRAC,CRDSNUM                                                 
         MVC   DLPSCRNM,CRDSNAME                                                
         MVI   DLPSEL,X'68'                                                     
         OI    DLPSTYPE,X'80'      IN CASE WE MISSED IT ON ELEMENT 2            
         SPACE 2                                                                
AT17A    DS    0H                                                               
         CLI   COSTBYTE,C' '                                                    
         BE    AT18                                                             
         LR    RF,R8                                                            
         AR    R8,R3                                                            
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(RF)                                                    
         MVI   DLPSEL,X'69'        DEBIT DEPT C/A CLIENT                        
         MVC   DLPSCRAC,COSTNUM                                                 
         MVC   DLPSCRNM,COSTNAME                                                
         MVC   DLPSDBAC,CRCNUM                                                  
         MVC   DLPSDBNM,CRCNAME                                                 
         CLI   FLAGDRCR,C'D'                                                    
         BE    AT17C                                                            
         MVC   DLPSDBAC,COSTNUM    OR DEBIT CLIENT C/A INCOME                   
         MVC   DLPSDBNM,COSTNAME                                                
         MVC   DLPSCRAC,CINUM                                                   
         MVC   DLPSCRNM,CINAME                                                  
AT17C    OI    DLPSTYPE,X'80'                                                   
         LA    R3,1(R3)            BACK TO DLPSLEN AGAIN                        
         LR    RF,R8                                                            
         AR    R8,R3                                                            
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(RF)                                                    
         MVI   DLPSEL,X'6A'        CREDIT CLIENT                                
         MVC   DLPSDBAC,CR13NUM                                                 
         MVC   DLPSDBNM,CR13NAME                                                
         CLI   FLAGDRCR,C'D'                                                    
         BE    AT17E                                                            
         MVC   DLPSDBAC,COSTNUM    OR CREDIT INCOME C/A CLIENT                  
         MVC   DLPSDBNM,COSTNAME                                                
         MVC   DLPSCRAC,CINUM                                                   
         MVC   DLPSCRNM,CINAME                                                  
AT17E    LA    R3,1(R3)                                                         
         SPACE 2                                                                
AT18     AR    R8,R3                                                            
         MVI   0(R8),0             END OF RECORD                                
         LA    R5,IOAREA-1                                                      
         SR    R8,R5                                                            
         STH   R8,HALF                                                          
         MVC   IOAREA(2),HALF                                                   
         BAS   RE,PUTDAY                                                        
         EJECT                                                                  
*              BUILD TWA1 RECORD & PUT IT THERE                                 
         SPACE 2                                                                
         XC    WORK,WORK                                                        
         IC    R3,ANTDOCH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),ANTDOC      REF                                          
         L     R3,DMCB+8                                                        
         MVC   WORK+10(4),0(R3)    DISK ADDRESS                                 
         BAS   RE,ADTWA1                                                        
         MVI   ERRNUM,X'FF'                                                     
         LA    R2,ANTDOCH                                                       
         B     EXIT                                                             
         SPACE 2                                                                
DLIST    DS    0CL2                                                             
*&&UK                                                                           
         DC    C'SFSISTSVSX'                                                    
*&&                                                                             
*&&US                                                                           
         DC    C'SSSQSPSNSWSYSISTSVSX'                                          
*&&                                                                             
         DC    X'FF'                                                            
         SPACE 2                                                                
CLIST    DS    0CL2                                                             
*&&UK                                                                           
         DC    C'SESJSR'                                                        
*&&                                                                             
*&&US                                                                           
         DC    C'SESJSR'                                                        
*&&                                                                             
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE ACBATCODE                                                      
       ++INCLUDE ACBATDSECT                                                     
       ++INCLUDE ACBATF4D                                                       
         EJECT                                                                  
PROGD    DSECT                                                                  
PRELOC   DS    F                                                                
STFSW    DS    CL1                                                              
COSTBYTE DS    CL1                                                              
DEPSW    DS    CL1                                                              
FLAGDRCR DS    CL1                                                              
DRNUM    DS    CL15                                                             
DRNAME   DS    CL36                                                             
CRNUM    DS    CL15                                                             
CRNAME   DS    CL36                                                             
CRPSNUM  DS    CL15                                                             
CRPSNAME DS    CL36                                                             
CRDSNUM  DS    CL15                                                             
CRDSNAME DS    CL36                                                             
CRCNUM   DS    CL15                                                             
CRCNAME  DS    CL36                                                             
COSTNUM  DS    CL15                                                             
COSTNAME DS    CL36                                                             
CINUM    DS    CL15                                                             
CINAME   DS    CL36                                                             
DEPNUM   DS    CL15                                                             
DEPNAME  DS    CL36                                                             
CR13NUM  DS    CL15                                                             
CR13NAME DS    CL36                                                             
STAFFNUM DS    CL15                                                             
STAFFNAM DS    CL36                                                             
SICONTRA DS    CL15                                                             
SICONTRN DS    CL36                                                             
OFFSW    DS    CL1                                                              
BYTE     DS    CL1                                                              
KEY      DS    CL49                                                             
IOAREA   DS    CL2000                                                           
PROGDX   DS    0C                                                               
         EJECT                                                                  
*        ACGENBOTH                                                              
*        ACGENDAY                                                               
*        DDFLDIND                                                               
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENDAY                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020ACBAT0B   05/01/02'                                      
         END                                                                    
