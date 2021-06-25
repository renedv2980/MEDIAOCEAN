*          DATA SET ACBAT33    AT LEVEL 046 AS OF 05/01/02                      
*PHASE T61B33A,+0                                                               
         TITLE 'T61B33 - INTER- AGENCY BILLING'                                 
T61B33   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYWORKX-MYWORKD,*BAT33*,R8,CLEAR=YES                             
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING MYWORKD,RC                                                       
         USING TWAD,RA                                                          
         SPACE 3                                                                
*                                                                               
         LA    R2,CMBDATH          VALIDATE THE DATE                            
         BAS   RE,ANY                                                           
         MVI   ERRNUM,INVDATE                                                   
         GOTO1 DATVAL,DMCB,(0,CMBDAT),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(1,PDAT)                                    
         GOTO1 DATECHK,DMCB,PDAT                                                
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         SPACE 1                                                                
         LA    R2,CMBDUEH          FOR CURSOR REPOSITION ON ERROR               
         XC    PDUEDAT,PDUEDAT                                                  
         CLI   CMBDUEH+5,0                                                      
         BE    DATX                                                             
         MVI   ERRNUM,INVDATE      DUE DATE                                     
         GOTO1 DATVAL,DMCB,(0,CMBDUE),PDUEDAT                                   
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR               BAD DATE                                     
DATX     DS    0H                                                               
*                                                                               
         EJECT ,                                                                
*              VALIDATE CLIENT                                                  
*                                                                               
CLI      LA    R2,CMBSYMH          SYSTEM/MEDIA IS ALWAY REQUIRED               
         BAS   RE,ANY                                                           
         LA    R5,COMPEL                                                        
         USING ACCOMPD,R5                                                       
         LA    R2,CMBCLIH          CLIENT                                       
         BAS   RE,ANY                                                           
         MVI   ERRNUM,INVCLI                                                    
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(41),SPACES                                                 
         MVC   KEY+1(2),ACMPJOB    U/L FOR PRODUCTION                           
         TM    CMBCLIH+4,X'20'                                                  
         BO    CLI2                                                             
         MVC   CMBCLIN,SPACES                                                   
         OI    CMBCLINH+6,X'80'                                                 
         NI    CMBPROH+4,X'DF'     MUST VALIDATE PRODUCT                        
         MVC   CMBPRON,SPACES                                                   
         OI    CMBPRONH+6,X'80'                                                 
         XC    CLIPROF,CLIPROF                                                  
         XC    PRODPROF,PRODPROF                                                
         XC    JOBPROF,JOBPROF                                                  
         SPACE 1                                                                
CLI2     ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),CMBCLI                                                  
         LA    R4,KEY+3                                                         
         IC    R3,CLILNGTH         LEVA LENGTH                                  
         AR    R4,R3                                                            
         SPACE 1                                                                
         LA    R6,CLIPROF                                                       
         BAS   RE,GETACC                                                        
         MVC   CLINUM,KEY                                                       
         MVC   CMBCLIN,ACCTNAME    CLIENT NAME                                  
         OI    CMBCLINH+6,X'80'                                                 
         OI    CMBCLIH+4,X'20'                                                  
         EJECT ,                                                                
*              VALIDATE THE PRODUCT                                             
*                                                                               
PROD     LA    R2,CMBPROH                                                       
         BAS   RE,ANY                                                           
         MVI   ERRNUM,INVPRO                                                    
         TM    CMBPROH+4,X'20'                                                  
         BO    PROD2                                                            
         MVC   CMBPRON,SPACES                                                   
         OI    CMBPRONH+6,X'80'                                                 
         XC    PRODPROF,PRODPROF                                                
         SPACE 1                                                                
PROD2    ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),CMBPRO                                                   
         LA    R6,PRODPROF                                                      
         BAS   RE,GETACC                                                        
         MVC   CMBPRON,ACCTNAME         PRODUCT NAME                            
         MVC   PRODNAM,ACCTNAME                                                 
         OI    CMBPRONH+6,X'80'                                                 
         OI    CMBPROH+4,X'20'                                                  
         SPACE 1                                                                
         MVC   PRODNUM,KEY         SAVE CLIENT/PROD KEY                         
         BAS   RE,PROFMERG         MERGE CLIENT/PROD PROFILES                   
         XR    R6,R6               NO MORE PROFILES                             
*                                                                               
         MVC   KEY(15),PRODNUM     DEAL WITH SALES ANALYSIS ELEMENT             
         BAS   RE,HIGH                                                          
         LA    R1,IOAREA                                                        
PROD6    CLI   0(R1),0                                                          
         BE    PRODX                                                            
         CLI   0(R1),X'3D'                                                      
         BE    PROD8                                                            
         ZIC   RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     PROD6                                                            
         USING ACSAND,R1                                                        
PROD8    MVC   KEY(15),ACSACODE                                                 
         MVC   PRODNUM,ACSACODE                                                 
         BAS   RE,GETACC                                                        
         MVC   PRODNAM,ACCTNAME                                                 
PRODX    DS    0H                                                               
         EJECT ,                                                                
*              VALIDATE THE INCOME AND COSTING ACCOUNTS                         
*    THE VALID ENTRIES IN THIS FIELD ARE : M1; M1,CC; M1,M2; M1,M2,CC.          
         SPACE 1                                                                
VALACC   LA    R2,CMBSYMH          SYSTEM/MEDIA                                 
         BAS   RE,ANY                                                           
         GOTO1 SCANNER,DMCB,(20,(R2)),(3,BLOCK)                                 
         MVI   ERRNUM,INVALID                                                   
         MVI   FNDX,1                                                           
         CLI   DMCB+4,0                                                         
         BE    EXIT                                                             
         SR    R4,R4                                                            
         IC    R4,DMCB+4           NUMBER OF INPUT FIELDS                       
         LA    R5,BLOCK                                                         
         USING SCAND,R5                                                         
         OC    CMBSYM,SPACES       MAKE X'00' SPACES                            
         MVI   MEDIANUM,0                                                       
         LA    R7,MEDIA1                                                        
*                                                                               
         USING MEDIAD,R7                                                        
VALACC2  CLI   SCLEN2,0            FIRST MUST BE MEDIA                          
         BE    VALACC4             NO RIGHT SIDE                                
         CLI   SCLEN1,2                                                         
         BNE   ERROR                                                            
         CLC   SCDATA1(2),=C'MI'                                                
         BNE   VALACC8                                                          
         TM    COMPSTA4,X'10'      USING MEDIA INTERFACE RECORDS                
         BNO   ERROR                                                            
         CLI   MEDIANUM,2          ALREADY HAVE 2 INCOME ACCOUNTS               
         BE    ERROR                                                            
         CLI   SCLEN2,2                                                         
         BNE   ERROR                                                            
         MVC   MEDIA(2),SCDATA2                                                 
         B     VALACC6                                                          
*                                                                               
VALACC4  CLI   SCLEN1,12                                                        
         BH    ERROR                                                            
         CLI   MEDIANUM,2                                                       
         BE    ERROR                                                            
         MVC   INCNUM(1),COMPANY                                                
         MVC   INCNUM+1(2),=C'SI'                                               
         MVC   INCNUM+3(12),SCDATA1                                             
*                                                                               
VALACC6  SR    R0,R0                                                            
         IC    R0,MEDIANUM                                                      
         AH    R0,=H'1'                                                         
         STC   R0,MEDIANUM                                                      
         LA    R7,MEDNXT(R7)                                                    
         B     VALACC10                                                         
*                                                                               
VALACC8  CLC   SCDATA1(2),=C'CC'                                                
         BNE   ERROR                                                            
         CLI   SCLEN2,3                                                         
         BH    ERROR                                                            
         CLI   CC,0                                                             
         BNE   ERROR                                                            
         MVC   CC,SCDATA2                                                       
         CLI   SCDATA1+2,C' '                                                   
         BE    VALACC10                                                         
         TM    SCDATA1+2,X'F0'     THIRD CHARACTER IN CC FIELD                  
         BNO   ERROR               MUST BE NUMERIC                              
         MVC   CCDSP,SCDATA1+2     GET DISPLACEMENT IN BINARY                   
         NI    CCDSP,X'0F'                                                      
*                                                                               
VALACC10 LA    R5,SCNLNQ(R5)        NEXT FIELD CAN BE CC=123                    
         SR    R0,R0                                                            
         IC    R0,FNDX                                                          
         AH    R0,=H'1'                                                         
         STC   R0,FNDX                                                          
         BCT   R4,VALACC2                                                       
*                                                                               
         MVI   FNDX,0                                                           
         MVI   SET,1               SET NUMBER                                   
         LA    R7,MEDIA1           DO FIRST INCOME ACCOUNT                      
         BAS   RE,SYSM             VALIDATE ALL ACCOUNTS                        
         CLI   ERRNUM,0                                                         
         BNE   SYSMERR                                                          
         SPACE 1                                                                
         CLI   MEDIANUM,1                                                       
         BE    VALACC12            ONLY ONE INCOME ACCOUNT                      
         MVI   SET,2                                                            
         LA    R7,MEDIA2           DO SECOND INCOME ACCOUNT                     
         BAS   RE,SYSM                                                          
         CLI   ERRNUM,0                                                         
         BNE   SYSMERR                                                          
         SPACE 1                                                                
VALACC12 LA    R7,MEDIA1                                                        
         MVC   WORK(36),INCNAM     PUT OUT INCOME ACCOUNT NAME                  
         MVC   WORK+36(37),SPACES                                               
         LA    R7,MEDIA2                                                        
         CLI   INCNAM,0                                                         
         BE    *+14                                                             
         MVI   WORK+36,C'/'                                                     
         MVC   WORK+37(36),INCNAM                                               
         GOTO1 SQUASHER,DMCB,WORK,73                                            
         MVC   CMBSYMN,WORK                                                     
         OI    CMBSYMNH+6,X'80'                                                 
         MVI   FNDX,0                                                           
         B     OFF01                                                            
         EJECT ,                                                                
*              SYSTEM/MEDIA VALIDATION                                          
*              SET=1, OR 2   THE FIRST OR SECOND MEDIA ACCOUNTS                 
         SPACE 1                                                                
         USING MEDIAD,R7                                                        
SYSM     NTR1                                                                   
         CLI   MEDIA,0                                                          
         BE    SYSM04                                                           
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'08'           GET MEDIA INTERFACE RECORD                   
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(2),MEDIA                                                   
         BAS   RE,HIGH                                                          
         MVI   ERRNUM,INVACC                                                    
         CLC   KEYSAVE,KEY                                                      
         BNE   SYSMERR             RECORD NOT FOUND                             
         MVI   ELCODE,X'19'                                                     
         LA    R4,KEY                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACMID,R4                                                         
         MVC   SOURCE,ACMIDESC             DESCRIPTION                          
         MVC   COSCONUM(1),COMPANY                                              
         MVC   COSCONUM+1(2),=C'12'                                             
         MVC   COSCONUM+3(12),ACMICOST   SAVE COSTING ACCOUNT                   
         MVC   MCNTLNUM+1(14),ACMICNTL     AND MEDIA CONTROL                    
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),ACMICOMM        SET INCOME ACCOUNT                     
         B     SYSM06                                                           
         SPACE 1                                                                
SYSM04   MVC   KEY,SPACES                                                       
         MVC   KEY(15),INCNUM                                                   
*                                                                               
SYSM06   LA    R2,CMBSYMH                                                       
         LA    R6,WORK             FORCE READ OF INCOME ACCOUNT                 
         BAS   RE,GETACC           GET THE RECORD                               
         SR    R6,R6                                                            
         BAS   RE,CHECKACC         CHECK FOR POSTING                            
         MVC   INCNUM,KEY          SAVE THE INCOME KEY                          
         MVC   INCNAM,ACCTNAME     ACCOUNT NAME                                 
*                                                                               
         CLI   MEDIA,0             MEDIA INTERFACE RECORDS                      
         BNE   SYSM09              ALREADY HAVE COST ACCOUNT                    
         MVC   COSCONUM,SPACES                                                  
         MVC   COSCONUM(1),COMPANY                                              
         MVC   COSCONUM+1(2),=C'12'                                             
         MVC   COSCONUM+3(1),ACCTCOST  COSTING COMMISSION CODE                  
         MVI   ELCODE,X'2C'                                                     
         L     R4,AIOAREA                                                       
         BAS   RE,GETEL                                                         
*                                                                               
SYSM08   BNE   SYSM09                                                           
         USING ACSPECD,R4                                                       
         CLI   ACSPTYP,ACSPOAN                                                  
         BE    *+12                                                             
         BAS   RE,NEXTEL                                                        
         B     SYSM08                                                           
         MVC   COSCONUM+3(12),ACSPACCT                                          
*                                                                               
SYSM09   CLI   CMBAPH+5,0          A/P OVERRIDE                                 
         BNE   SYSM10              IF USED NO NEED TO VERIFY HERE               
         MVC   KEY,SPACES          CHECK MEDIA CONTROL                          
         MVC   KEY(15),MCNTLNUM                                                 
         MVC   KEY(1),COMPANY                                                   
         CLI   MCNTLNUM+1,C' '    MEDIA CONTROL ACCOUNT FROM MI                 
         BH    *+16                                                             
         MVC   KEY(15),INCNUM                                                   
         MVC   KEY+1(2),=C'SZ'                                                  
         BAS   RE,GETACC           GET THE RECORD                               
         SR    R6,R6                                                            
         BAS   RE,CHECKACC         CHECK FOR POSTING                            
         MVC   MCNTLNUM,KEY         SAVE THE SZ KEY                             
         MVC   MCNTLNAM,ACCTNAME   ACCOUNT NAME                                 
         SPACE 1                                                                
SYSM10   LA    R4,PROFILE                                                       
         USING ACPROFD,R4                                                       
         MVC   CLICOST,ACPRCOST   CLIENT COSTING                                
         DROP  R4                 KEEP IT CLEAN                                 
         LA    R1,CLICOST                                                       
*                                                                               
         CLI   CC,0                WAS CLIENT COST OVERRIDE ENTERED ?           
         BE    SYSM12              NO, LOOK FOR STATUS                          
         CLC   MEDIANUM,SET        BYPASS IF NOT AT LAST MEDIA                  
         BNE   SYSM12                                                           
         CLI   CCDSP,0             START POSITION  NOT SPECFIED                 
         BE    SYSM11              DEFAULT IS 7                                 
         SR    R3,R3                                                            
         IC    R3,CCDSP                                                         
         LA    R4,2(R3,R1)          START AT LEDGER                             
         B     *+8                                                              
*                                                                               
SYSM11   LA    R4,7(0,R1)          R4 POINTS TO POSITION IN COST KEY            
         LA    R3,3                MAX NUMBER OF ENTRIES                        
         LA    R5,CC               R5 POINTS TO OVERRIDE DATA                   
         B     SYSM15                                                           
*                                                                               
SYSM12   L     R5,AIOAREA          INCOME ACCOUNT IN IOAREA                     
         AH    R5,DATADISP                                                      
*                                                                               
SYSM13   CLI   0(R5),0                                                          
         BE    SYSM16                                                           
         CLI   0(R5),X'30'         STATUS ELEMENT                               
         BE    SYSM14                                                           
         ZIC   R4,1(R5)                                                         
         AR    R5,R4                                                            
         B     SYSM13                                                           
         SPACE 1                                                                
         USING ACSTATD,R5                                                       
SYSM14   OC    ACSTCNTR,SPACES                                                  
         CLI   ACSTCPOS,0          IF 0 START AT AT KEY+7                       
         BE    *+20                                                             
         LA    R4,2(R1)            ADDR OF KEY+2                                
         ZIC   R0,ACSTCPOS         STARTING POINT NUMBER                        
         AR    R4,R0               NEW STARTING POINT                           
         B     *+8                                                              
         LA    R4,7(R1)            CLIENT COSTING KEY+7                         
         LA    R3,3                                                             
         LA    R5,ACSTCNTR                                                      
         SPACE 1                                                                
SYSM15   CLI   0(R5),C' '          REPLACE ONLY NON BLANK                       
         BE    *+10                                                             
         MVC   0(1,R4),0(R5)                                                    
         LA    R5,1(R5)                                                         
         LA    R4,1(R4)                                                         
         BCT   R3,*-22                                                          
         SPACE 1                                                                
SYSM16   TM    COMPSTAT,X'10'      COSTING REQUIRED                             
         BZ    SYSM17              NO SKIP COSTING                              
         SPACE 1                                                                
         MVC   KEY,SPACES          NO CHECK COMMISSION ACCT                     
         MVC   KEY(15),COSCONUM                                                 
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   COSCONAM,ACCTNAME   COMMISSION ACCOUNT NAME                      
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),CLICOST     CHECK CLIENT COSTING ACCOUNT                 
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   CLICTNM,ACCTNAME    CLIENT COST ACCOUNT NAME                     
*                                                                               
         LA    R1,MEDIA1                                                        
         CR    R1,R7               IF NOT FIRST ACCOUNT GET OUT                 
         BNE   SYSMX                                                            
         MVC   KEY,SPACES                                                       
         MVC   COSBLNUM,COSCONUM   CHECK COSTING BILLING                        
         MVI   COSBLNUM+2,C'1'                                                  
         MVC   KEY(15),COSBLNUM                                                 
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   COSBLNAM,ACCTNAME                                                
         SPACE 1                                                                
SYSM17   DS    0H                                                               
         SPACE 1                                                                
SYSMX    MVI   ERRNUM,0            NO ERRORS                                    
*                                                                               
SYSMERR  XMOD1 1                                                                
         EJECT ,                                                                
*              VALIDATE OFFICE                                                  
*                                                                               
OFF01    LA    RF,PROFILE                                                       
         USING ACPROFD,RF                                                       
         MVC   SVANAL,SPACES                                                    
         MVC   SVANAL,ACPROFFC   USE ACPRUNIT FOR                               
         MVC   CMBOFFN,SPACES                                                   
         OI    CMBOFFNH+6,X'80'                                                 
         LA    R2,CMBOFFH            ANALYSIS                                   
         OC    CMBOFF,SPACES                                                    
         CLI   5(R2),0               UNLESS OFFICE IS                           
         BNE   OFF02                 SPECIFIED                                  
         TM    COMPSTAT,X'20'      IS OFFICE REQUIRED                           
         BZ    BILN                NO OFFICE INPUT IS OK                        
         BAS   RE,ANY              OFFICE REQUIRED                              
         GOTO1 AVALOFFC,DMCB,(X'80',CMBOFF)                                     
         CLI   ERRNUM,OK                                                        
         BNE   ERROR                                                            
         SPACE 1                                                                
OFF02    MVC   SVANAL,CMBOFF                                                    
         LA    RF,PROFILE                                                       
         CLI   ACPROFFC,C' '       CHECK OFFICE ENTERED AGAINST OFFICE          
         BNH   OFF03               ON CLIENT RECORD - IF NOT EQUAL              
         CLC   ACPROFFC,CMBOFF       REJECT THE INPUT                           
         BE    OFF03                                                            
         MVC   MSG,=CL60'ERROR INVALID OFFICE CODE'                             
         MVI   ERRNUM,X'FE'                                                     
         B     EXIT                                                             
OFF03    TM    COMPSTAT,X'20'      OFFICE REQUIRED                              
         BZ    BILN                                                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'2D'     YES,  MUST VALIDATE                          
         MVC   KEY+3(2),CMBOFF                                                  
         XR    R6,R6                                                            
         BAS   RE,GETACC                                                        
         MVC   CMBOFFN,ACCTNAME                                                 
         OI    CMBOFFNH+6,X'80'                                                 
         DROP  R7,RF               KEEP IT CLEAN                                
         EJECT ,                                                                
*              VALIDATE BILL NUMBER AND AMOUNT FIELDS                           
*                                                                               
BILN     LA    R2,CMBBILH                                                       
         BAS   RE,ANY                                                           
         MVC   BILNUM,SPACES                                                    
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   BILNUM(0),CMBBIL                                                 
         SPACE 1                                                                
*              VALIDATE AMOUNT FIELDS                                           
*                                                                               
         ZAP   AMT,=P'0'           INITIALIZE ALL CASH FIELDS                   
         ZAP   NET,=P'0'                                                        
         ZAP   SPL,=P'0'                                                        
         ZAP   CD,=P'0'                                                         
         ZAP   ORD,=P'0'                                                        
         ZAP   PAY,=P'0'                                                        
         LA    R4,MEDIA1                                                        
         USING MEDIAD,R4                                                        
         ZAP   COM,=P'0'                                                        
         LA    R4,MEDNXT(R4)                                                    
         ZAP   COM,=P'0'                                                        
*                                                                               
AMT01    LA    R2,CMBAMTH          BILL AMOUNT                                  
         BAS   RE,ANY                                                           
         ZIC   R3,5(R2)                                                         
         LA    RF,8(R2)                                                         
         BAS   RE,VALCASH                                                       
         ZAP   AMT,0(8,RF)                                                      
         SPACE 1                                                                
AMT03    LA    R2,CMBNETH          NET AMOUNT                                   
         BAS   RE,ANY                                                           
         ZIC   R3,5(R2)                                                         
         LA    RF,8(R2)                                                         
         BAS   RE,VALCASH                                                       
         ZAP   NET,0(8,RF)                                                      
         SPACE 1                                                                
AMT05    LA    R2,CMBCOMH          COMMISSION                                   
         BAS   RE,ANY                                                           
         LA    RF,8(R2)            SEPARATED BY A COMMA                         
         XR    R3,R3                                                            
         SPACE 1                                                                
AMT07    CLI   0(RF),C','          LOOK FOR COMMA OR SPACE                      
         BE    AMT09                                                            
         CLI   0(RF),X'41'                                                      
         BL    AMT09                                                            
         LA    R3,1(R3)            LENGTH IN R3                                 
         LA    RF,1(RF)                                                         
         B     AMT07                                                            
         SPACE 1                                                                
AMT09    LA    RF,8(R2)            RF TO DATA                                   
         BAS   RE,VALCASH          VALIDATE FIRST AMOUNT                        
         LA    R4,MEDIA1                                                        
         ZAP   COM,0(8,RF)         SAVE COMMISSION                              
         ZIC   RF,5(R2)                                                         
         CR    R3,RF               LENGTH IS EQUAL TO HEADER                    
         BE    AMT11               ONE AMOUNT                                   
         SPACE 1                                                                
         MVI   FNDX,2                                                           
         LA    R4,MEDNXT(R4)       R4 TO NEXT MEDIA ACCOUNT                     
         MVI   ERRNUM,INVALID                                                   
         CLI   INCNUM,0            IF NO SECOND INCOME                          
         BE    ERROR               SECOND AMOUNT IS INVALID                     
         MVI   FNDX,0                                                           
         LA    RF,9(R3,R2)         RF TO SECOND                                 
         ZIC   R5,5(R2)            TOTAL LENGTH                                 
         SR    R5,R3               LESS FIRST                                   
         BCTR  R5,0                LESS ONE                                     
         LTR   R3,R5               R3 TO LENGTH OF SECOND                       
         BNP   ERROR                                                            
         BAS   RE,VALCASH                                                       
         ZAP   COM,0(8,RF)         SAVE SECOND COMMISSION FIELD                 
         B     AMT13                                                            
*                                                                               
AMT11    LA    R4,MEDNXT(R4)       TO SECOND MEDIA FIELDS                       
         CLI   INCNUM,0            DID I HAVE SECOND INCOME                     
         BE    AMT13               IT NOT IT'S OK                               
         MVI   ERRNUM,NOINPUT      IF YES, I'M MISSING SECOND AMOUNT            
         B     ERROR                                                            
*                                                                               
AMT13    MVI   ERRNUM,INVAMNT      VERIFY TOTAL                                 
         ZAP   DUB,NET                                                          
         LA    R4,MEDIA1                                                        
         AP    DUB,COM                                                          
         LA    R4,MEDNXT(R4)                                                    
         AP    DUB,COM             AMT = NET + COMMISSION                       
         CP    AMT,DUB                                                          
         BNE   ERROR                                                            
         SPACE 1                                                                
         LA    R2,CMBCDH           VALID CASH DISCOUNT                          
         ZIC   R3,5(R2)                                                         
         LTR   R3,R3                                                            
         BZ    AMT15               NO CASH DISCOUNT                             
         LA    RF,8(R2)                                                         
         BAS   RE,VALCASH                                                       
         ZAP   CD,0(8,RF)                                                       
         SPACE 1                                                                
AMT15    ZAP   SPL,AMT            SPECIAL GROSS = BILL AMOUNT                   
         LA    R2,CMBSPLH          UNLESS SPECIAL IS INPUT                      
         ZIC   R3,5(R2)                                                         
         LTR   R3,R3                                                            
         BZ    AMTXX               NO SPECIAL AMOUNT                            
         LA    RF,8(R2)                                                         
         BAS   RE,VALCASH                                                       
         ZAP   SPL,0(8,RF)                                                      
         SPACE 1                                                                
AMTXX    DS    0H                                                               
         EJECT ,                                                                
*              VALIDATE BILLING SOURCE, ESTIMATE NUMBER AND MOS                 
*                                                                               
BILSRC   DS    0H                                                               
         MVC   SOURCE,SPACES                                                    
         LA    R2,CMBBLSRH         BILLING SOURCE                               
         CLI   5(R2),0                                                          
         BE    ESTN                                                             
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   SOURCE(0),CMBBLSR                                                
         CLI   SOURCE,X'41'                                                     
         BL    ERROR                                                            
         EJECT ,                                                                
*                                                                               
ESTN     LA    R2,CMBESTH          VALIDATE ESTIMATE NUMBER                     
         MVI   ERRNUM,INVALID                                                   
         CLI   5(R2),0                                                          
         BE    ESTNX               NO ESTIMATE NUMBER                           
         LA    R6,EL23                                                          
         USING ACOTHERD,R6                                                      
         CLI   ACOTNUM,0                                                        
         BNE   ERROR               PREVIOUSLY USED                              
         MVC   ACOTNUM,SPACES                                                   
         MVC   ACOTPROF,SPACES                                                  
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ACOTNUM(0),CMBEST                                                
         MVI   ACOTPROF,C'J'       ASSUME JOB                                   
         LA    R3,1(R3)                                                         
         CH    R3,=H'3'                                                         
         BH    ESTNX               IF LENGTH MORE THAN 3                        
         LA    R1,ACOTNUM          SPOT / PRINT MUST BE NUMERIC                 
ESTN04   CLI   0(R1),C'9'                                                       
         BH    ESTN09                                                           
         CLI   0(R1),C'0'                                                       
         BL    ESTN09                                                           
         LA    R1,1(R1)                                                         
         BCT   R3,ESTN04                                                        
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  HALF(2),ACOTNUM(0)                                               
         CLI   CMBSYM,C'S'                                                      
         BE    ESTN06                                                           
         CLI   CMBSYM,C'P'                                                      
         BE    ESTN08                                                           
         B     ESTN09                                                           
ESTN06   CP    HALF,=P'255'       HIGHEST SPOT ESTIMATE NUMBER IS 255           
         BH    ESTN09                                                           
         MVC   ACOTPROF(2),CMBSYM                                               
         B     ESTNX                                                            
ESTN08   CP    HALF,=P'999'       HIGHEST PRINT ESTIMATE NUMBER IS 999          
         BH    ESTN09                                                           
         MVC   ACOTPROF(2),CMBSYM                                               
         B     ESTNX                                                            
ESTN09   MVI   ACOTPROF,C'X'                                                    
ESTNX    DS    0H                                                               
         EJECT ,                                                                
*                                                                               
MOS01    LA    R2,CMBMOSH          VALIDATE MONTH OF SERVICE                    
         CLI   5(R2),0                                                          
         BE    MOSX                NO MONTH                                     
         LA    R6,EL23                                                          
         CLI   ACOTDATE,X'40'                                                   
         BH    ERROR                                                            
         MVI   ERRNUM,INVDATE                                                   
         GOTO1 DATVAL,DMCB,(2,CMBMOS),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   ACOTDATE,WORK+6                                                  
         CLC   ACOTDATE,=X'8101'                                                
         BL    ERROR                                                            
         MVC   ADVMOS,ACOTDATE                                                  
MOSX     DS    0H                                                               
         EJECT ,                                                                
*              VALIDATE RECEIVABLE CODE                                         
*                                                                               
REC01    LA    RF,PROFILE                                                       
         USING ACPROFD,RF                                                       
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),ACPRRECV    RECEIVABLE CODE FROM PROFILE                 
         DROP  RF                  KEEP IT CLEAN                                
         LA    R2,CMBARH                                                        
         MVC   CMBARN,SPACES                                                    
         OI    CMBARNH+6,X'80'                                                  
         CLI   5(R2),0             A/R ACCOUNT INPUT                            
         BE    REC10               NO USE ACPRRECV                              
**T                                                                             
         CLC   8(2,R2),=C'SE'                                                   
         BNE   REC04                                                            
         MVC   MSG,=CL60'ERROR - INVALID UNIT/LEDGER FOR POSTING'               
         MVI   ERRNUM,X'FE'                                                     
         B     ERROR                                                            
**T                                                                             
REC04    MVC   KEY(1),COMPANY      ELSE USE                                     
         MVC   KEY+1(14),SPACES    THE ACCOUNT                                  
         ZIC   R3,5(R2)            THAT IS INPUT                                
         BCTR  R3,0                                                             
         CLI   8(R2),C'*'                                                       
         BE    REC05                                                            
         EX    R3,*+8                                                           
         B     REC13                                                            
         MVC   KEY+1(0),CMBAR                                                   
         SPACE 1                                                                
REC05    MVC   KEY(15),PRODNUM         SCHLITZ SPECIAL CODE                     
         LTR   R3,R3                                                            
         BZ    ERROR                                                            
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+9(0),CMBAR+1    PUT 1-BYTE A/R WITH CLI/PRODUCT              
         MVC   KEY+1(2),=C'SR'     TO MAKE A RECEIVABLE ACCOUNT                 
         B     REC13                                                            
*                                                                               
REC10    LA    R2,CMBCLIH          POINT TO CLIENT IF ACPRREVC                  
REC13    BAS   RE,GETACC           ELSE R2 POINTS                               
         BAS   RE,CHECKACC                                                      
         LA    R2,CMBARH                                                        
         CLI   5(R2),0                                                          
         BE    REC15                                                            
         MVC   CMBARN,ACCTNAME     IF A/R ACCOUNT PUT OUT NAME                  
         OI    CMBARNH+6,X'80'                                                  
*                                                                               
REC15    MVC   ARNUM,KEY           SAVE A/R ACCOUNT OR ACPRREVC                 
         MVC   ARNAM,ACCTNAME                                                   
         EJECT ,                                                                
*              VALIDATE A/P ACCOUNT                                             
*                                                                               
APA01    LA    R2,CMBAPH           DEAL WITH A/P OVERRIDE                       
         CLI   5(R2),0                                                          
         BE    APAX                                                             
         MVC   KEY+1(14),SPACES                                                 
         MVC   KEY+1(2),=C'SE'     ASSUME U/L=SE                                
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),CMBAP                                                   
         CLI   CMBAP,C'*'                                                       
         BNE   APA05                                                            
         MVI   ERRNUM,INVALID                                                   
         CLI   CMBAP+1,C'S'                                                     
         BE    APA03                                                            
         CLI   CMBAP+1,C'G'                                                     
         BNE   ERROR                                                            
APA03    BCTR  R3,0                                                             
         MVC   KEY+1(14),SPACES                                                 
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+1(0),CMBAP+1                                                 
APA05    BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         TM    ACCTSTAT,X'48'     NO DEPT / STAFF                               
         BNZ   ERROR                                                            
         CLI   ACCOST,C' '        OR ANALYSIS POSTINGS                          
         BNE   ERROR                                                            
         MVC   APNUM,ACCTNUM                                                    
         MVC   APNAM,ACCTNAME                                                   
         OI    CMBAPH+4,X'20'                                                   
         MVC   CMBAPN,ACCTNAME                                                  
         OI    CMBAPNH+6,X'80'                                                  
APAX     DS    0H                                                               
         EJECT ,                                                                
*              DEAL WITH SPECIAL ELEMENTS                                       
*                                                                               
NUMS01   LA    R2,CMBNUMH                                                       
         CLI   CMBNUMH+5,0                                                      
         BE    NUMS26                                                           
         GOTO1 SCANNER,DMCB,(20,(R2)),(6,BLOCK)                                 
         MVI   ERRNUM,INVALID                                                   
         MVI   FNDX,1                                                           
         CLI   DMCB+4,0                                                         
         BE    ERROR                                                            
         MVC   FNDN,DMCB+4           NUMBER OF INPUT FIELDS                     
         LA    R5,BLOCK                                                         
         USING SCAND,R5                                                         
*                                                                               
         USING ACOTHERD,R6                                                      
NUMS03   CLC   SCDATA1(2),=C'SN'   SPECIAL NUMBER                               
         BNE   NUMS05                                                           
         LA    R6,EL23                                                          
         CLI   ACOTNUM,0                                                        
         BNE   ERROR               ALREADY USED                                 
         CLI   SCLEN2,9            MAXIMUM 9 CHARS.                             
         BH    ERROR                                                            
         MVC   ACOTNUM,SCDATA2                                                  
         B     NUMS25                                                           
*                                                                               
NUMS05   CLC   SCDATA1(2),=C'DA'   SPECIAL DATE FOR RECEIVABLES                 
*        BNE   NUMS07                                                           
         B     NUMS07                                                           
         LA    R6,EL23                                                          
         CLI   ACOTDATE,0                                                       
         BNE   ERROR                                                            
         MVI   ERRNUM,INVDATE                                                   
         GOTO1 DATVAL,DMCB,(2,SCDATA2),WORK                                     
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   ACOTDATE,WORK+6                                                  
         MVI   ERRNUM,INVALID                                                   
         B     NUMS25                                                           
         SPACE 1                                                                
         USING ACABILLD,R6                                                      
NUMS07   LA    R6,EL27                                                          
         CLC   SCDATA1(2),=C'EA'   EA NUMBER                                    
         BNE   NUMS09                                                           
         CLI   SCLEN2,10           HARD CODE FOR LENGTH OF 10                   
         BNE   ERROR                                                            
         MVI   ERRNUM,6            INVALID FORMAT                               
         CLI   SCDATA2+5,C'-'      AND FOR HYPHEN PLACEMENT                     
         BNE   ERROR                                                            
         CLI   SCDATA2+8,C'-'                                                   
         BNE   ERROR                                                            
         MVI   ERRNUM,INVNUM                                                    
         MVC   WORK(L'ACABEANO),=20X'F0' AND FOR ALL NUMERIC                    
         MVZ   WORK(5),SCDATA2                                                  
         MVZ   WORK+5(2),SCDATA2+6                                              
         MVZ   WORK+7(1),SCDATA2+9                                              
         CLC   WORK(L'ACABEANO),=20X'F0'                                        
         BNE   ERROR                                                            
         MVI   ERRNUM,INVALID                                                   
         CLI   ACABEANO,0                                                       
         BNE   ERROR                                                            
         MVC   ACABEANO,SPACES                                                  
         MVC   ACABEANO(10),SCDATA2      MOVE IN 10 ONLY                        
         B     NUMS25                                                           
*                                                                               
NUMS09   CLC   SCDATA1(2),=C'AC'   ACCOUNT NUMBER                               
         BNE   NUMS15                                                           
         CLI   SCLEN2,13           HARD CODE FOR LENGTH OF 13                   
         BNE   ERROR                                                            
         MVC   WORK(L'ACABACNO),=20X'F0' CHECK FOR ALL NUMERIC                  
         MVI   ERRNUM,6            INVALID FORMAT                               
         CLI   SCDATA2+4,C'-'      AND FOR HYPHEN PLACEMENT                     
         BNE   ERROR                                                            
         MVZ   WORK(4),SCDATA2                                                  
         CLI   SCDATA2+9,C'-'      SCHEME 1                                     
         BNE   NUMS11                                                           
         MVZ   WORK+4(4),SCDATA2+5                                              
         MVZ   WORK+8(3),SCDATA2+10                                             
         B     NUMS13                                                           
NUMS11   CLI   SCDATA2+8,C'-'      SCHEME 2                                     
         BNE   ERROR                                                            
         MVZ   WORK+4(3),SCDATA2+5                                              
         MVZ   WORK+7(4),SCDATA2+9                                              
NUMS13   MVI   ERRNUM,INVNUM                                                    
         CLC   WORK(L'ACABACNO),=20X'F0'                                        
         BNE   ERROR                                                            
         MVI   ERRNUM,INVALID                                                   
         CLI   ACABACNO,0                                                       
         BNE   ERROR                                                            
         MVC   ACABACNO,SPACES                                                  
         MVC   ACABACNO(13),SCDATA2 HARD CODE FOR MOVE                          
         B     NUMS25                                                           
         SPACE 1                                                                
NUMS15   CLC   SCDATA1(3),=C'ENO'  ESTIMATE NUMBER                              
*        BNE   NUMS17                                                           
         B     NUMS17                                                           
         CLI   SCLEN2,L'ACABESNO   CAN BE ANYTHING                              
         BH    ERROR                                                            
         MVI   ERRNUM,INVALID                                                   
         CLI   ACABESNO,0                                                       
         BNE   ERROR                                                            
         MVC   ACABESNO,SCDATA2                                                 
         B     NUMS25                                                           
         SPACE 1                                                                
NUMS17   CLC   SCDATA1(3),=C'BUD'  BUDGET NUMBER                                
         BNE   ERROR                                                            
         CLI   SCLEN2,L'ACABBUNO   MUST BE FULL LENGTH                          
         BNE   ERROR                                                            
         MVI   ERRNUM,INVNUM                                                    
         TM    SCVAL2,X'80'        AND MUST BE NUMERIC                          
         BZ    ERROR                                                            
         MVI   ERRNUM,INVALID                                                   
         CLI   ACABBUNO,0                                                       
         BNE   ERROR                                                            
         MVC   ACABBUNO,SCDATA2                                                 
         SPACE 1                                                                
NUMS25   LA    R5,SCNLNQ(R5)       BUMP TO NEXT FIELD                           
         ZIC   RE,FNDX                                                          
         AH    RE,=H'1'                                                         
         STC   RE,FNDX                                                          
         CLC   FNDX,FNDN                                                        
         BNH   NUMS03                                                           
         MVI   FNDX,0                                                           
*                                                                               
         USING ACOTHERD,R6                                                      
NUMS26   LA    R6,EL23             VALIDATE OTHERS ELEMENT                      
         CLI   ACOTNUM,0           NUMBER IS REQUIRED IF DATE ENTERED           
         BNE   NUMS30                                                           
         CLI   ACOTDATE,0                                                       
         BE    NUMS33                                                           
         LA    R2,CMBESTH                                                       
         MVC   MSG(L'NUMSERR1),NUMSERR1                                         
         MVI   ERRNUM,X'FE'                                                     
         B     ERROR                                                            
NUMSERR1 DC    C'** ERROR ** ESTIMATE REQUIRED '                                
*                                                                               
NUMS30   MVC   ACOTEL(2),=X'230F'  FINISH BUILDING IT                           
         CLI   ACOTPROF,X'40'                                                   
         BH    NUMS33                                                           
         MVC   ACOTPROF(1),CMBSYM                                               
         CLI   CMBSYM,C'S'         SPOT                                         
         BE    NUMS33                                                           
         CLI   CMBSYM,C'P'         PRINT                                        
         BE    NUMS33                                                           
         MVI   ACOTPROF,C'X'                                                    
*                                                                               
NUMS33   LA    R6,EL27             YES, FILL IN REST OF ELEMENT                 
         USING ACABILLD,R6                                                      
         OC    EL27,EL27                                                        
         BZ    NUMSX                                                            
         MVC   ACABEL(2),=X'2739'                                               
         OC    ACABESNO(L'ACABESNO+L'ACABBUNO),ACABESNO                         
         BNZ   *+8                                                              
         MVI   ACABLEN,X'1E'       SMALL EL. IF NO ESNO OR BUNO                 
NUMSX    DS    0H                                                               
         EJECT ,                                                                
*              VALIDATE INTER-AGENCY FIELDS                                     
*                                                                               
AOR01    DS    0H                  GET INCOME ACCOUNT FOR SELL-OFF              
         LA    R4,MEDIA1                                                        
         USING MEDIAD,R4                                                        
         MVC   CMBAGYN,SPACES                                                   
         OI    CMBAGYNH+6,X'80'                                                 
         MVC   SOINCNUM,INCNUM    FIRST PICK UP DEFAULT INCOME ACCOUNT          
         MVC   SOINCNAM,INCNAM    AND NAME                                      
         MVC   AORCONUM,COSCONUM                                                
         MVC   AORCONAM,COSCONAM                                                
         LA    R2,CMBAGYH                                                       
         CLI   5(R2),0                                                          
         BE    PAY01               NO INCOME ACCOUNT                            
         CLC   CMBAGY(3),=C'MI='                                                
         BNE   AOR07                                                            
         MVI   ERRNUM,INVALID                                                   
         CLI   CMBAGYH+5,5         IF MI MUST BE 5 LONG                         
         BNE   ERROR                                                            
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'08'           GET MEDIA INTERFACE RECORD                   
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(2),CMBAGY+3                                                
         BAS   RE,HIGH                                                          
         MVI   ERRNUM,INVACC                                                    
         CLC   KEYSAVE,KEY                                                      
         BNE   ERROR               RECORD NOT FOUND                             
         MVI   ELCODE,X'19'                                                     
         LA    R4,KEY                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACMID,R4                                                         
         MVC   AORCONUM(1),COMPANY                                              
         MVC   AORCONUM+1(2),=C'12'                                             
         MVC   AORCONUM+3(12),ACMICOST   SAVE COSTING ACCOUNT                   
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),ACMICOMM                                               
         SR    R6,R6                                                            
         BAS   RE,GETACC           GET THE RECORD                               
         BAS   RE,CHECKACC         CHECK FOR POSTING                            
         MVC   SOINCNUM,KEY          SAVE THE INCOME KEY                        
         MVC   SOINCNAM,ACCTNAME     AND NAME                                   
         MVC   CMBAGYN,ACCTNAME                                                 
         B     AOR09                                                            
*                                                                               
AOR07    MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'SI'                                                  
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),CMBAGY     CHECK THE INCOME ACCOUNT                     
         SR    R6,R6                                                            
         BAS   RE,GETACC           GET THE RECORD                               
         BAS   RE,CHECKACC         CHECK FOR POSTING                            
         MVC   SOINCNUM,KEY          SAVE THE INCOME KEY                        
         MVC   SOINCNAM,ACCTNAME     AND NAME                                   
         MVC   CMBAGYN,ACCTNAME                                                 
*                                                                               
         MVC   AORCONUM,SPACES     BUILD COMMISSION ACCOUNT KEY                 
         MVC   AORCONUM(1),COMPANY                                              
         MVC   AORCONUM+1(2),=C'12'                                             
         MVC   AORCONUM+3(1),ACCTCOST    COSTING COMMISSION CODE                
         MVI   ELCODE,X'2C'                                                     
         L     R4,AIOAREA                                                       
         BAS   RE,GETEL                                                         
*                                                                               
AOR08    BNE   AOR10                                                            
         USING ACSPECD,R4                                                       
         CLI   ACSPTYP,ACSPOAN                                                  
         BE    *+12                                                             
         BAS   RE,NEXTEL                                                        
         B     AOR08                                                            
         MVC   AORCONUM+3(12),ACSPACCT                                          
*                                                                               
AOR09    MVC   KEY,SPACES                 CHECK COMMISSION ACCT                 
         MVC   KEY(15),AORCONUM                                                 
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   AORCONAM,ACCTNAME           COMMISSION ACCOUNT NAME              
*                                                                               
AOR10    LA    R6,EL23                                                          
         USING ACOTHERD,R6                                                      
         MVC   ACOTPROF(2),SOINCNUM+3      SAVE INCOME ACCOUNT CODE             
         CLC   CMBAGY(3),=C'MI='                                                
         BNE   *+10                                                             
         MVC   ACOTPROF(2),CMBAGY+3                                             
         EJECT ,                                                                
*                                   VALIDATE PAYABLE ACCOUNT                    
*                                                                               
PAY01    MVC   CMBPAYN,SPACES                                                   
         OI    CMBPAYNH+6,X'80'                                                 
         LA    R2,CMBPAYAH                                                      
         BAS   RE,ANY           IF INCOME ACCOUNT MUST HAVE PAYABLE             
PAY03    MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'SX'                                                  
         CLC   TWAAGY,=C'NW'       FOR AYER DEFAULT IS SS                       
         BNE   *+10                                                             
         MVC   KEY+1(2),=C'SS'                                                  
         ZIC   R3,5(R2)                                                         
         LA    RE,KEY+3                                                         
         LA    RF,CMBPAYA                                                       
         CLI   0(RF),C'*'          U/L OVERRIDE                                 
         BNE   PAY06                                                            
         BCTR  R3,0                SKIP THE *                                   
         LA    RF,1(RF)                                                         
         LA    RE,KEY+1                                                         
         CLC   0(2,RF),=C'SS'                                                   
         BE    PAY06                                                            
         CLC   0(2,RF),=C'SP'                                                   
         BE    PAY06                                                            
         CLC   0(2,RF),=C'SU'                                                   
         BE    PAY06                                                            
         CLC   0(2,RF),=C'SX'                                                   
         BE    PAY06                                                            
         CLC   0(2,RF),=C'SV'                                                   
         BE    PAY06                                                            
         MVI   ERRNUM,INVALID                                                   
         B     ERROR                                                            
*                                                                               
PAY06    BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)       PAYABLE ACCOUNT                              
         SR    R6,R6                                                            
         BAS   RE,GETACC           GET THE RECORD                               
         BAS   RE,CHECKACC         CHECK FOR POSTING                            
         MVC   SOPAYNUM,KEY          SAVE THE PAYABLE KEY                       
         MVC   SOPAYNAM,ACCTNAME     AND NAME                                   
         MVC   CMBPAYN,ACCTNAME                                                 
         EJECT ,                                                                
*              VALIDATE ORDERED AMOUNT AND PAYABLE AMOUNT                       
*                                                                               
ORD01    DS    0H                                                               
         ZAP   ORD,AMT             DEFAULT IS AMOUNT                            
         LA    R2,CMBORDH          ORDERED AMOUNT                               
         CLI   5(R2),0                                                          
         BE    PAYAMT              NO ORDERED AMOUNT INPUT                      
         ZIC   R3,5(R2)                                                         
         LA    RF,8(R2)                                                         
         BAS   RE,VALCASH                                                       
         ZAP   ORD,0(8,RF)         SAVE ORDERED AMOUNT                          
*                                                                               
PAYAMT   DS    0H                                                               
         LA    R2,CMBPAYH          PAYABLE AMOUNT/PERCENT                       
         CLI   5(R2),0                                                          
         BNE   PAYAMT02            BRANCH IF INPUT                              
         CLI   CMBPAYAH+5,0        IF PAYABLE ACCOUNT                           
         BE    PAYAMTX                                                          
         BAS   RE,ANY              MUST HAVE AMOUNT                             
         B     ERROR                                                            
*                                                                               
PAYAMT02 MVI   ERRNUM,INVALID                                                   
         CLI   CMBPAYAH+5,0        IF NO PAYABLE ACCOUNT                        
         BE    ERROR               THIS IS INVALID                              
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         LA    R3,CMBPAY(R3)       R1 TO LAST CHARACTER OF INPUT                
         CLI   0(R3),C'%'                                                       
         BE    PAYAMT05            INPUT PERCENT                                
         ZIC   R3,5(R2)                                                         
         LA    RF,8(R2)                                                         
         BAS   RE,VALCASH                                                       
         ZAP   PAY,0(8,RF)         SAVE PAYABLE AMOUNT                          
         B     PAYAMT07                                                         
*                                                                               
PAYAMT05 MVI   ERRNUM,INVAMNT      VALID PERCENT                                
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         LTR   R3,R3                                                            
         BZ    ERROR                                                            
         GOTO1 CASHVAL,DMCB,(4,CMBPAY),(R3)                                     
         OC    4(4,R1),4(R1)                                                    
         BZ    ERROR                                                            
         L     RF,4(R1)                                                         
         CVD   RF,DUB                                                           
         ZAP   PL16,DUB                                                         
         MP    PL16,ORD            ORDERED X PERCENT                            
         SRP   PL16,64-6,5         ROUNDED                                      
         ZAP   PAY,PL16            SAVE PAYABLE AMOUNT                          
*                                                                               
PAYAMT07 LA    R2,CMBESTH          ESTIMATE IS REQUIRED                         
         CLI   5(R2),0             IF PAYABLE AMOUNT IS INPUT                   
         BNE   *+8                                                              
         BAS   RE,ANY                                                           
*                                                                               
         LA    R2,CMBMOSH          SO IS MOS                                    
         CLI   5(R2),0                                                          
         BNE   *+8                                                              
         BAS   RE,ANY                                                           
*                                                                               
PAYAMTX  DS    0H                                                               
         EJECT ,                                                                
POST     LA    R7,IOAREA+2                                                      
         USING DLDESCD,R7                                                       
         MVI   DLDSEL,X'64'                                                     
         MVC   DLDSREF,BILNUM                                                   
         MVC   DLDSDATE,PDAT                                                    
         MVI   DLDSSBRF,0                                                       
         XC    DLDSSTAT(7),DLDSSTAT                                             
         SPACE 1                                                                
         LA    R2,CMBNARH                                                       
         LA    R3,DLDSNARR                                                      
         XC    DLDSNARR,DLDSNARR                                                
         BAS   RE,NARRSCAN                                                      
         SPACE 1                                                                
         LA    R5,DLDSNARR                                                      
         SR    R5,R7               ELEMENT - NARRATIVE                          
         AH    R5,=H'2'                                                         
         AR    R5,R6               R6 = L'NARRATIVE                             
         STH   R5,HALF                                                          
         MVC   IOAREA(2),HALF                                                   
         SH    R5,=H'2'                                                         
         STH   R5,HALF                                                          
         MVC   DLDSLEN,HALF+1                                                   
         ZIC   R3,DLDSLEN                                                       
         AR    R7,R3                                                            
         EJECT ,                                                                
*              BUILD RECEIVABLE POSTING                                         
*                                                                               
         CLI   EL23,0                                                           
         BE    POST05              NO 23 ELEMENT                                
         LA    R6,EL23             MOVE 23 ELEMENT TO POSTING RECORD            
         ZIC   R3,1(R6)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R6)                                                    
         LA    R7,1(R3,R7)                                                      
*                                                                               
POST05   CLI   EL27,0                                                           
         BE    POST07              NO 27 ELEMENT                                
         LA    R6,EL27             MOVE 27 ELEMENT TO POSTING RECORD            
         ZIC   R3,1(R6)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R6)                                                    
         LA    R7,1(R3,R7)                                                      
*                                                                               
         USING TRCASHD,R7                                                       
POST07   CP    CD,=P'0'                                                         
         BE    POST09                                                           
         MVC   TRCSEL(2),=X'5009'  BUILD CASH DISCOUNT ELEMENT                  
         MVI   TRCSTYPE,C'D'                                                    
         ZAP   TRCSAMNT,CD                                                      
         IC    R3,TRCSLEN                                                       
         AR    R7,R3                                                            
         SPACE 1                                                                
POST09   LA    R4,MEDIA1                                                        
         USING MEDIAD,R4                                                        
         USING TRCASHD,R7                                                       
         MVC   TRCSEL(2),=X'5009'  INCOME TO RECEIVABLE                         
         MVI   TRCSTYPE,C'I'       POSTING                                      
         ZAP   TRCSAMNT,COM                                                     
         IC    R3,TRCSLEN                                                       
         AR    R7,R3                                                            
         EJECT ,                                                                
***************************************                                         
         USING ACMTD,R7                                                         
         XC    ACMTEL(ACMTLNQ),ACMTEL                                           
         MVI   ACMTEL,ACMTELQ            ELEM CODE                              
         MVI   ACMTLEN,ACMTLNQ           ELEM LENGTH                            
         MVC   ACMTSYS(2),CMBSYM         SYSTEM/MEDIA                           
         MVC   ACMTCLI,CMBCLI            CLIENT                                 
         OC    ACMTCLI,SPACES                                                   
         MVC   ACMTPRD,CMBPRO            PRODUCT                                
         OC    ACMTPRD,SPACES                                                   
*                                                                               
         LA    R4,MEDIA1                                                        
         CLI   MEDIA,C' '          SYSTEM/MEDIA FROM MI RECORD                  
         BNH   *+10                                                             
         MVC   ACMTSYS,MEDIA                                                    
         USING MEDIAD,R4                                                        
         ZAP   DUB,COM                                                          
         LA    R4,MEDNXT(R4)                                                    
         CLI   INCNUM,0                                                         
         BE    *+10                                                             
         AP    DUB,COM                   ADD SECOND INCOME                      
         CVB   RF,DUB                    COMMISSION                             
         STCM  RF,15,ACMTCOM                                                    
         CVB   RF,SPL                    GROSS BILLING                          
         STCM  RF,15,ACMTGRS                                                    
         CVB   RF,NET                    NET BILLING                            
         STCM  RF,15,ACMTNET                                                    
         CVB   RF,CD                     CASH DISCOUNT                          
         STCM  RF,15,ACMTCD                                                     
         CVB   RF,AMT                    RECEIVABLE                             
         STCM  RF,15,ACMTRECV                                                   
         MVC   ACMTMOS,ADVMOS            ADV MOS  OR BATCH MOS                  
         OC    ACMTMOS,ACMTMOS           TO MT ELEMENT                          
         BNZ   *+10                                                             
         MVC   ACMTMOS,PMOS                                                     
         MVC   ACMTJOB,CMBEST      ESTIMATE NUMBER                              
         OC    ACMTJOB,SPACES                                                   
         MVC   ACMTDSCP,SPACES     NO DESCRIPTION                               
         MVC   SAV1A,ACMTEL        SAVE THE 1A ELEMENT                          
         SR    R3,R3                                                            
         IC    R3,ACMTLEN                                                       
         AR    R7,R3                                                            
*                                                                               
*                                                                               
******************************                                                  
         USING TRDUED,R7                                                        
POST11   OC    PDUEDAT,PDUEDAT     IF DUEDATE CREATE A 61                       
         BZ    POST13                                                           
         MVC   TRDUEL(2),=X'6104'                                               
         GOTO1 DATCON,DMCB,(0,PDUEDAT),(2,TRDUDATE)                             
         IC    R3,TRDUEN                                                        
         AR    R7,R3                                                            
         EJECT ,                                                                
         USING DLPOSTD,R7                                                       
POST13   DS    0H                                                               
         LA    R4,MEDIA1                                                        
         MVC   DLPSEL(2),=X'6971'  DEBIT RECEIVABLES                            
         MVC   DLPSDBAC,ARNUM                                                   
         MVC   DLPSDBNM,ARNAM                                                   
         MVC   DLPSCRAC,SPACES                                                  
         MVC   DLPSCRNM,SPACES                                                  
         MVI   DLPSTYPE,0          SUBSIDIARY                                   
         ZAP   DLPSAMNT,AMT                                                     
         MVC   DLPSANAL,SVANAL                                                  
         LA    R2,WORK                                                          
         GOTO1 CHOPPER,DMCB,(36,INCNAM),(12,(R2)),(0,1)                         
         SPACE 1                                                                
         MVC   DLPSCRAC+3(12),WORK                                              
         CLI   SOURCE,X'41'        DID THEY OVERRIDE SOURCE                     
         BL    POST15              IF NOT USE INCOME ACCT NAME                  
         MVC   DLPSCRAC+3(12),SOURCE  ELSE USE OVERRIDE                         
POST15   CLC   DLPSDBAC+1(2),=C'SX'     IF A/R ACCOUNT SX MAKE IT               
         BNE   POST17                   A MINUS CREDIT                          
         MVI   DLPSEL,X'6A'                                                     
         MP    DLPSAMNT,=P'-1'                                                  
         MVC   DLPSDBAC,DLPSCRAC                                                
         MVC   DLPSDBNM,DLPSCRNM                                                
         MVC   DLPSCRAC,ARNUM                                                   
         MVC   DLPSCRNM,ARNAM                                                   
POST17   ZIC   R3,DLPSLEN                                                       
         AR    R7,R3                                                            
         EJECT ,                                                                
*               CREDIT THE  A/P ACCOUNTS                                        
*                                                                               
         CP    NET,=PL8'0'         NET ZERO NO PAYABLE                          
         BE    POST21              POSTING                                      
         MVC   DLPSEL(2),=X'6A71'  CREDIT PAYABLE                               
         MVC   DLPSDBAC,CLINUM     CONTRA IS CLIENT                             
         MVC   DLPSDBNM,CMBCLIN                                                 
         OC    DLPSDBNM,SPACES                                                  
         MVC   DLPSCRAC,MCNTLNUM                                                
         MVC   DLPSCRNM,MCNTLNAM                                                
         OC    APNUM,APNUM         A/P OVERRIDE                                 
         BZ    POST19                                                           
         MVC   DLPSCRAC,APNUM                                                   
         MVC   DLPSCRNM,APNAM                                                   
         SPACE 1                                                                
POST19   MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,NET                                                     
         MVC   DLPSANAL,SVANAL                                                  
         IC    R3,DLPSLEN                                                       
         AR    R7,R3                                                            
         EJECT ,                                                                
*              CREDIT THE INCOME ACCOUNT(S)                                     
*                                                                               
         USING TRCASHD,R7                                                       
POST21   MVC   TRCSEL(2),=X'5009'                                               
         MVI   TRCSTYPE,C'G'       GROSS                                        
         ZAP   TRCSAMNT,SPL        SPECIAL AMOUNT                               
         IC    R3,TRCSLEN                                                       
         AR    R7,R3                                                            
*****************************                                                   
         MVC   0(ACMTLNQ,R7),SAV1A   ADD 1A ELEMENT TO POSTINGS                 
         USING ACMTD,R7                                                         
         LA    R4,MEDIA1                                                        
         USING MEDIAD,R4                                                        
         ZAP   DUB,COM                                                          
         CVB   RF,DUB                 COMMISSION                                
         STCM  RF,15,ACMTCOM                                                    
         SR    R3,R3                                                            
         IC    R3,ACMTLEN                                                       
         AR    R7,R3                                                            
****************************                                                    
         SPACE 1                                                                
         USING DLPOSTD,R7                                                       
         MVC   DLPSEL(2),=X'6A71'  CREDIT INCOME                                
         MVC   DLPSDBAC,PRODNUM    CONTRA IS CLIENT PRODUCT                     
         MVC   DLPSDBNM,PRODNAM                                                 
         LA    R4,MEDIA1                                                        
         USING MEDIAD,R4                                                        
         MVC   DLPSCRAC,INCNUM                                                  
         MVC   DLPSCRNM,INCNAM                                                  
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,COM                                                     
         MVC   DLPSANAL,SVANAL                                                  
         IC    R3,DLPSLEN                                                       
         AR    R7,R3                                                            
*                                                                               
*              SECOND MEDIA(INCOME) ACCOUNT                                     
POST25   LA    R4,MEDNXT(R4)                                                    
         CLI   INCNUM,0                                                         
         BE    POST27              NO SECOND INCOME ACCOUNT                     
         USING TRCASHD,R7                                                       
         MVC   TRCSEL(2),=X'5009'                                               
         MVI   TRCSTYPE,C'G'       GROSS=ZERO                                   
         ZAP   TRCSAMNT,=P'0'                                                   
         IC    R3,TRCSLEN                                                       
         AR    R7,R3                                                            
*****************************                                                   
         MVC   0(ACMTLNQ,R7),SAV1A   ADD 1A ELEMENT TO POSTINGS                 
         USING ACMTD,R7                                                         
         ZAP   DUB,COM                                                          
         CVB   RF,DUB                 COMMISSION                                
         STCM  RF,15,ACMTCOM                                                    
         XC    ACMTGRS,ACMTGRS                                                  
         XC    ACMTNET,ACMTNET                                                  
         XC    ACMTCD,ACMTCD                                                    
         XC    ACMTRECV,ACMTRECV                                                
         SR    R3,R3                                                            
         IC    R3,ACMTLEN                                                       
         AR    R7,R3                                                            
*****************************                                                   
         SPACE 1                                                                
         USING DLPOSTD,R7                                                       
         MVC   DLPSEL(2),=X'6A71'  CREDIT INCOME                                
         MVC   DLPSDBAC,PRODNUM    CONTRA IS CLIENT PRODUCT                     
         MVC   DLPSDBNM,PRODNAM                                                 
         MVC   DLPSCRAC,INCNUM                                                  
         MVC   DLPSCRNM,INCNAM                                                  
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,COM                                                     
         MVC   DLPSANAL,SVANAL                                                  
         IC    R3,DLPSLEN                                                       
         AR    R7,R3                                                            
         EJECT ,                                                                
*              NOW DO THE COSTING POSTINGS                                      
*                                                                               
POST27   TM    COMPSTAT,X'10'      COSTING REQUIRED                             
         BZ    POST30              NO, ADD RECORD                               
         SPACE 1                                                                
         USING DLPOSTD,R7                                                       
         LA    R4,MEDIA1                                                        
         USING MEDIAD,R4                                                        
         MVC   DLPSEL(2),=X'6871'  DEBIT AND CREDIT                             
         MVC   DLPSDBAC,CLICOST                                                 
         MVC   DLPSDBNM,CLICTNM                                                 
         MVC   DLPSCRAC,COSCONUM                                                
         MVC   DLPSCRNM,COSCONAM                                                
         MVI   DLPSTYPE,X'80'                                                   
         ZAP   DLPSAMNT,COM                                                     
         MVC   DLPSANAL,SVANAL                                                  
         IC    R3,DLPSLEN                                                       
         AR    R7,R3                                                            
         SPACE 2                                                                
         USING DLPOSTD,R7                                                       
         MVC   DLPSEL(2),=X'6871'                                               
         MVC   DLPSDBAC,CLICOST                                                 
         MVC   DLPSDBNM,CLICTNM                                                 
         MVC   DLPSCRAC,COSBLNUM                                                
         MVC   DLPSCRNM,COSBLNAM                                                
         MVI   DLPSTYPE,X'80'                                                   
         ZAP   DLPSAMNT,SPL                                                     
         MVC   DLPSANAL,SVANAL                                                  
         IC    R3,DLPSLEN                                                       
         AR    R7,R3                                                            
         SPACE 1                                                                
         LA    R4,MEDNXT(R4)                                                    
         CLI   INCNUM,0                                                         
         BE    POST30              ONLY ONE INCOME ACCOUNT                      
         USING DLPOSTD,R7                                                       
         MVC   DLPSEL(2),=X'6871'                                               
         MVC   DLPSDBAC,CLICOST                                                 
         MVC   DLPSCRAC,COSCONUM                                                
         MVC   DLPSCRNM,COSCONAM                                                
         MVI   DLPSTYPE,X'80'                                                   
         ZAP   DLPSAMNT,COM                                                     
         MVC   DLPSANAL,SVANAL                                                  
         IC    R3,DLPSLEN                                                       
         AR    R7,R3                                                            
         EJECT ,                                                                
*              AOR - SELLOFF POSTINGS                                           
*                                                                               
POST30   CP    PAY,=P'0'                                                        
         BE    POST90              NO SELLOFF                                   
         USING TRCASHD,R7                                                       
         MVC   TRCSEL(2),=X'5009'  DEBIT TO INCOME                              
         MVI   TRCSTYPE,C'G'       MEMO GROSS = 0                               
         ZAP   TRCSAMNT,=P'0'                                                   
         IC    R3,TRCSLEN                                                       
         AR    R7,R3                                                            
*****************************                                                   
         MVC   0(ACMTLNQ,R7),SAV1A   ADD 1A ELEMENT TO POSTINGS                 
         USING ACMTD,R7                                                         
         ZAP   DUB,PAY                                                          
         MP    DUB,=P'-1'                                                       
         CVB   RF,DUB                 COMMISSION                                
         STCM  RF,15,ACMTCOM                                                    
         XC    ACMTGRS,ACMTGRS                                                  
         XC    ACMTNET,ACMTNET                                                  
         XC    ACMTCD,ACMTCD                                                    
         XC    ACMTRECV,ACMTRECV                                                
         SR    R3,R3                                                            
         IC    R3,ACMTLEN                                                       
         AR    R7,R3                                                            
*****************************                                                   
         SPACE 1                                                                
         USING DLPOSTD,R7                                                       
POST33   DS    0H                                                               
         MVC   DLPSEL(2),=X'6A71'  MINUS CREDIT TO INCOME                       
         MVC   DLPSCRAC,SOINCNUM                                                
         MVC   DLPSCRNM,SOINCNAM                                                
         MVC   DLPSDBAC,PRODNUM    CONTRA IS CLIENT PRODUCT                     
         MVC   DLPSDBNM,PRODNAM                                                 
         MVI   DLPSTYPE,0          SUBSIDIARY                                   
         ZAP   DLPSAMNT,PAY                                                     
         MP    DLPSAMNT,=P'-1'                                                  
         MVC   DLPSANAL,SVANAL                                                  
         ZIC   R3,DLPSLEN                                                       
         AR    R7,R3                                                            
*                                                                               
*                               CREDIT PAYABLE                                  
POST37   DS    0H                                                               
         CLI   EL23,0                                                           
         BE    POST38              NO 23 ELEMENT                                
         LA    R6,EL23             MOVE 23 ELEMENT TO POSTING RECORD            
         ZIC   R3,1(R6)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R6)                                                    
         LA    R7,1(R3,R7)                                                      
*                                                                               
         USING TRPAYD,R7                                                        
POST38   MVC   TRPYEL(2),=X'4655'  EXTRA PAY ELEMENT                            
         ZAP   TRPYCD,=P'0'        NO C.D.                                      
         MVC   TRPYCLI,SPACES                                                   
         MVC   TRPYCLI,CMBCLIN     CLIENT NAME                                  
         OC    TRPYCLI,SPACES                                                   
         MVC   TRPYPROD,SPACES                                                  
         MVC   TRPYPROD,CMBPRON     PRODUCT CODE                                
         OC    TRPYPROD,SPACES                                                  
         MVC   TRPYINV,SPACES                                                   
         MVC   TRPYINV(L'BILNUM),BILNUM    INVOICE NUMBER                       
         OC    TRPYINV,SPACES                                                   
         MVC   TRPYPER,SPACES                                                   
         GOTO1 DATCON,DMCB,(1,PDAT),(X'20',TRPYPER)                             
         MVC   TRPYPER+6(6),TRPYPER                                             
         MVI   TRPYTYPE,C'1'                                                    
         XC    TRPYEST(5),TRPYEST                                               
         IC    R3,TRPYLEN                                                       
         AR    R7,R3                                                            
*                                                                               
         USING TRCASHD,R7                                                       
         MVC   TRCSEL(2),=X'5009'                                               
         MVI   TRCSTYPE,C'G'       MEMO = GROSS ORDERED                         
         ZAP   TRCSAMNT,ORD                                                     
         IC    R3,TRCSLEN                                                       
         AR    R7,R3                                                            
*                                                                               
         USING DLPOSTD,R7                                                       
         MVC   DLPSEL(2),=X'6A71'  CREDIT PAYABLE                               
         MVC   DLPSCRAC,SOPAYNUM                                                
         MVC   DLPSCRNM,SOPAYNAM                                                
         MVC   DLPSDBAC,PRODNUM     CONTRA IS CLIENT/PRODUCT                    
         MVC   DLPSDBNM,PRODNAM                                                 
         CLI   SOPAYNUM+2,C'S'                                                  
         BNE   POST39                                                           
         MVC   DLPSDBAC,SPACES     IF PAYABLE IS THE SPOT LEDGER                
         MVC   DLPSDBAC+12(3),CMBCLI   CONTRA ACCOUNT IS CLIENT                 
         OC    DLPSDBAC+12(3),SPACES                                            
         MVC   DLPSDBNM,SPACES                                                  
         MVC   DLPSDBNM,CMBCLIN     CLIENT NAME                                 
         OC    DLPSDBNM,SPACES                                                  
POST39   MVI   DLPSTYPE,0                                                       
         MVC   DLPSANAL,SVANAL                                                  
         ZAP   DLPSAMNT,PAY                                                     
         ZIC   R3,DLPSLEN                                                       
         AR    R7,R3                                                            
         EJECT ,                                                                
*                                                                               
*              COSTING POSTINS FOR SELL OFF                                     
POST40   TM    COMPSTAT,X'10'      COSTING REQUIRED                             
         BZ    POST90              NO, ADD RECORD                               
         SPACE 1                                                                
         USING DLPOSTD,R7                                                       
         LA    R4,MEDIA1                                                        
         MVC   DLPSEL(2),=X'6871'  DEBIT AND CREDIT                             
         MVC   DLPSDBAC,CLICOST                                                 
         MVC   DLPSDBNM,CLICTNM                                                 
         MVC   DLPSCRAC,AORCONUM                                                
         MVC   DLPSCRNM,AORCONAM                                                
         MVI   DLPSTYPE,X'80'                                                   
         ZAP   DLPSAMNT,PAY                                                     
         MP    DLPSAMNT,=P'-1'                                                  
         MVC   DLPSANAL,SVANAL                                                  
         IC    R3,DLPSLEN                                                       
         AR    R7,R3                                                            
         EJECT ,                                                                
POST90   MVI   0(R7),0             END OF RECORD                                
         LA    R7,1(R7)                                                         
         LA    R3,IOAREA           GET LENGTH                                   
         SR    R7,R3                                                            
         STH   R7,HALF                                                          
         MVC   IOAREA(2),HALF                                                   
         BAS   RE,PUTDAY                                                        
         SPACE 1                                                                
         SR    R3,R3                                                            
         XC    WORK,WORK                                                        
         MVC   WORK(6),BILNUM                                                   
         ZAP   TRANSAMT,AMT                                                     
         MVC   WORK+6(4),FULL                                                   
         L     R3,DMCB+8                                                        
         MVC   WORK+10(4),0(R3)                                                 
         BAS   RE,ADTWA1                                                        
         SPACE 1                                                                
         LA    R2,CMBDATH                                                       
         MVI   ERRNUM,X'FF'                                                     
         B     EXIT                                                             
         EJECT ,                                                                
VALCASH  ST    RE,SAVRE                                                         
         MVI   ERRNUM,25           INVALID AMOUNT                               
         GOTO1 AMTVAL,DMCB,(RF),(R3)                                            
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         L     RF,DMCB+4                                                        
         LA    RF,0(RF)                                                         
         L     RE,SAVRE                                                         
         BR    RE                                                               
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT ,                                                                
       ++INCLUDE ACBATCODE                                                      
         LTORG                                                                  
         EJECT ,                                                                
*              LOCAL WORKING STORAGE                                            
         SPACE 1                                                                
MYWORKD  DSECT                                                                  
BLOCK    DS    10CL42                                                           
EL23     DS    CL25                                                             
EL27     DS    CL100                                                            
CLINUM   DS    CL15                                                             
PRODNUM  DS    CL15                                                             
PRODNAM  DS    CL36                                                             
ARNUM    DS    CL15                                                             
ARNAM    DS    CL36                                                             
APNUM    DS    CL15                                                             
APNAM    DS    CL36                                                             
SOINCNUM DS    CL15                SELLOFF - INCOME ACCOUNT  CODE               
SOINCNAM DS    CL36                                          NAME               
SOPAYNUM DS    CL15                          PAYABLE ACCOUNT CODE               
SOPAYNAM DS    CL36                                          NAME               
AORCONUM DS    CL15                          COSTING ACCOUNT CODE               
AORCONAM DS    CL36                                          NAME               
SVANAL   DS    CL2                                                              
SOURCE   DS    CL12                BILLING SOURCE OVERRIDE                      
FNDN     DS    CL1                                                              
AMT      DS    D                   BILL AMOUNT                                  
NET      DS    D                   NET AMOUNT                                   
SPL      DS    D                   SPECIAL                                      
CD       DS    D                   CASH DISCOUNT                                
PAY      DS    D                   PAYABLE AMOUNT(SELL-OFFS)                    
ORD      DS    PL6                 ORDERED AMOUNT                               
PL16     DS    PL16                WORK                                         
PDAT     DS    CL3                 PACKED DATE                                  
BILNUM   DS    CL6                                                              
PDUEDAT  DS    CL6                                                              
ADVMOS   DS    CL2                                                              
SAVRE    DS    F                                                                
MEDIANUM DS    CL1                                                              
SET      DS    CL1                                                              
ELCODE   DS    CL1                                                              
SAV1A    DS    CL(ACMTLNQ)                                                      
MEDIA1   DS    CL(MEDNXT)          CODES/NAMES BY SYSTEM/MEDIA(MEDIAD)          
MEDIA2   DS    CL(MEDNXT)                                                       
KEY      DS    CL49                                                             
IOAREA   DS    2000C                                                            
MYWORKX  DS    0C                                                               
         EJECT ,                                                                
*              DSECT TO COVER ACCOUNT CODES AND NAMES BY MEDIA                  
MEDIAD   DSECT                                                                  
MEDIA    DS    CL2                 MEDIA CODE                                   
INCNUM   DS    CL15                INCOME ACCOUNT          CODE                 
INCNAM   DS    CL36                                        NAME                 
CLICOST  DS    CL15                CLIENT COSTING ACCOUNT  CODE                 
CLICTNM  DS    CL36                                        NAME                 
COSCONUM DS    CL15                COSTING COMMISSIONS(12) CODE                 
COSCONAM DS    CL36                                        NAME                 
COSBLNUM DS    CL15                COSTING BILLING(11)     CODE                 
COSBLNAM DS    CL36                                        NAME                 
MCNTLNUM DS    CL15                MEDIA-CONTROL(SZ)       CODE                 
MCNTLNAM DS    CL36                                        NAME                 
COM      DS    PL6                 COMMISSION(INCOME) AMOUNT                    
CC       DS    CL7                 COSTING OVERRIDE(CC=)                        
CCDSP    DS    CL1                 COSTING OVERRIDE DISPLACEMENT                
MEDNXT   EQU   *-MEDIAD                                                         
         EJECT ,                                                                
*              DSECT FOR 42-BYTE SCAN BLOCK                                     
         SPACE 1                                                                
SCAND    DSECT                                                                  
SCLEN1   DS    CL1                                                              
SCLEN2   DS    CL1                                                              
SCVAL1   DS    CL1                                                              
SCVAL2   DS    CL1                                                              
SCBIN1   DS    CL4                                                              
SCBIN2   DS    CL4                                                              
SCDATA1  DS    CL10                                                             
SCDATA2  DS    CL20                                                             
SCNLNQ   EQU   *-SCAND                                                          
         EJECT ,                                                                
       ++INCLUDE ACBATDSECT                                                     
       ++INCLUDE ACBATCBD                                                       
         ORG   CONTABH                                                          
       ++INCLUDE ACBATD1D                                                       
         EJECT ,                                                                
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*        ACGENDAY                                                               
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
         PRINT OFF                                                              
*        DDFLDIND                                                               
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046ACBAT33   05/01/02'                                      
         END                                                                    
