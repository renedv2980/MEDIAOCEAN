*          DATA SET ACBAT21    AT LEVEL 012 AS OF 01/27/04                      
*PHASE T61B21A                                                                  
         TITLE 'T61B21 - BILLABLE TIME SHEETS'                                  
T61B21   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,*BAT21*,R8,CLEAR=YES,RR=R2                          
         USING PROGD,RC                                                         
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING TWAD,RA                                                          
         ST    R2,RELO1                                                         
         SPACE 1                                                                
*                                                                               
* BUILD SCREEN DIRECTORY                                                        
*                                                                               
* CANADIAN SCREEN MATCHES US SCREEN THROUGH SUPPLIER AMOUNT FIELD               
* SCREENS DIVERGE BELOW SUPPLIER AMOUNT                                         
*                                                                               
         LA    R1,USTAB                                                         
         CLI   AGYCTRY,CTRYUSA     TEST FOR US                                  
         BE    *+8                                                              
         LA    R1,CANTAB           NO-ITS CANADA                                
*                                                                               
BA1      CLI   0(R1),X'FF'         TEST FOR EOT                                 
         BE    BA1A                                                             
         LM    RE,RF,0(R1)                                                      
         LA    RE,TWAD(RE)         FORM READ ADDRESS OF SCREEN FIELD            
         LA    RF,PROGD(RF)        FORM ADDRESS OF ADCON                        
         ST    RE,0(RF)                                                         
         LA    R1,L'USTAB(R1)                                                   
         B     BA1                                                              
         SPACE 1                                                                
BA1A     DS    0H                                                               
         MVC   TMPMODE,MODE                                                     
         NI    TMPMODE,X'0F'                                                    
         CLI   TMPMODE,3                                                        
         BE    BA2A                                                             
         LA    R2,BITDOCH                                                       
         BAS   RE,ANY                                                           
         MVC   DOCSAVE,BITDOC                                                   
         OC    DOCSAVE,SPACES                                                   
         LA    R2,BITDATH                                                       
         MVI   ERRNUM,INVDATE                                                   
         BAS   RE,GETODAY                                                       
         CLI   BITDATH+5,0                                                      
         BE    BA2                                                              
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         CLC   WORK(2),=C'70'                                                   
         BL    ERROR                                                            
         SPACE 1                                                                
BA2      GOTO1 DATCON,DMCB,(0,WORK),(1,DATE)                                    
         EJECT                                                                  
BA2A     CLI   CSACT,ACTCHA                                                     
         BNE   *+16                                                             
         MVI   CLEAROK,C'N'        DON'T CLEAR GST/PST FOR CHANGE               
         MVI   USERPROV,C'Y'       USER ENTERED PROV FROM CHANGE                
         MVI   USERPST,C'Y'        USER ENTERED PST TYPE                        
*                                                                               
         CLI   AGYCTRY,CTRYCAN                                                  
         BNE   BA3X                                                             
         CLI   MODE,3                                                           
         BE    BA3X                                                             
         CLI   CLEAROK,C'N'        NOT OK TO CLEAR?                             
         BE    BA3G                                                             
         L     R2,AGORNH                                                        
         TM    4(R2),X'80'         INPUT THIS TIME?                             
         BO    BA3C                ONLY CLEAR THE OLD ONES                      
         OI    6(R2),X'80'                                                      
         MVI   8(R2),0                                                          
         L     R2,AGSTXH                                                        
         MVC   8(L'BICGSTX,R2),SPACES        CLEAR INFO DATA TOO                
         OI    6(R2),X'80'                                                      
         L     R2,ATYPNH                                                        
         MVC   8(L'BICTYPN,R2),SPACES                                           
         OI    6(R2),X'80'                                                      
*                                                                               
BA3C     L     R2,ATYPEH                                                        
         TM    4(R2),X'80'                                                      
         BO    *+12                                                             
         MVI   8(R2),0                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R2,AGAMTH                                                        
         TM    4(R2),X'80'                                                      
         BO    *+18                                                             
         OI    6(R2),X'80'                                                      
         MVI   5(R2),0                                                          
         XC    8(L'BICGAMT,R2),8(R2)                                            
         XC    XTRAELM,XTRAELM     CLEAR PST FIELDS                             
*                                                                               
         L     RF,BCAUTL                                                        
         TM    TSTAT6-UTLD(RF),TST6STRO        STEREO?                          
         BZ    BA3F                                                             
         L     R2,APROVH                                                        
         TM    4(R2),X'80'         NEW PROVINCE?                                
         BO    BA3J                                                             
         MVI   USERPROV,C'N'                                                    
         B     BA3K                                                             
*                                                                               
BA3F     LA    R2,BICSUPH          SUPPLIER                                     
         TM    4(R2),X'80'                                                      
         BNO   BA3G                                                             
         XC    VENDPROV,VENDPROV                                                
         L     R2,APROVH                                                        
         TM    4(R2),X'80'                                                      
         BO    BA3G                                                             
         XC    8(2,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
BA3G     L     R2,APROVH                                                        
         TM    4(R2),X'80'         NEW PROVINCE?                                
         BNO   BA3X                SAVE PROVINCE                                
BA3J     MVI   USERPROV,C'Y'       USER ENTERED PROVINCE                        
BA3K     MVC   KEEPPROV,8(R2)                                                   
         XC    XTRAELM,XTRAELM                                                  
*                                                                               
BA3X     MVI   CLEAROK,C'N'        DON'T CLEAR UNTIL DONE                       
*--------------------------------------------------------------------           
*        MODE=X'03'  CANADIAN SALES TAX SCREEN                                  
*--------------------------------------------------------------------           
         CLI   CSACT,ACTCHA        ITEM CHANGE?                                 
         BNE   BA4                                                              
         CLI   PFKEY,X'FF'         FIRST TIME, IT SETS PFKEY TO FF              
         BNE   *+8                 YES                                          
         MVI   PFKEY,0                                                          
BA4      CLI   MODE,3              CANDIAN TAX?                                 
         BE    BA6                                                              
*                                                                               
         CLI   PFKEY,7             USE PF=7 TO LOAD                             
         BNE   BA5                                                              
         CLI   AGYCTRY,CTRYCAN                                                  
         BE    BA4A                                                             
         MVC   FVMSGNO,=Y(AE$CSNA)   CANADA SCREEN N/A                          
         LA    R2,CONACTH                                                       
         B     ERROR                                                            
*                                                                               
BA4A     ZAP   SUPNET,=P'0'                                                     
         ZAP   SUPCASH,=P'0'                                                    
         ZAP   GST,=P'0'                                                        
         ZAP   PST,=P'0'                                                        
         B     BA8                                                              
*                                                                               
BA5      CLI   PFKEY,0                                                          
         BE    BA8                                                              
         MVI   ERRNUM,251          INVALID PFKEY                                
         L     R2,TIACURS                                                       
         B     ERROR                                                            
*                                                                               
BA6      CLI   AGYCTRY,CTRYCAN     CANADIAN?                                    
         BNE   BA8                                                              
         L     RF,=A(CTAXMOD)                                                   
         L     RE,RELO1                                                         
         AR    RF,RE                                                            
         MVI   CSSPROG,2                                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         GOTO1 (RF),DMCB,(RC)                                                   
         CLI   CSACT,ACTCHA        ARE WE DOING A CHANGE?                       
         BNE   CURSIT              NO, LEAVE                                    
         CLC   FVMSGNO,=AL2(FVFOK) IF ERROR, RETURN                             
         BNE   CURSIT                                                           
         MVC   FVMSGNO,=X'FFFF'    TELL BT61 WE HAVE TO COME BACK               
         B     CURSIT                                                           
         EJECT                                                                  
*              DEAL WITH CASH FIELDS                                            
*                                                                               
BA8      MVI   ERRNUM,0                                                         
         LA    R2,BITJAMTH                                                      
         BAS   RE,ANY                                                           
         BAS   RE,CASHCHK                                                       
         CLI   ERRNUM,INVAMNT                                                   
         BE    ERROR                                                            
         ZAP   JOBCASH,DUB                                                      
         ZAP   SUPCASH,=P'0'                                                    
         ZAP   GST,=P'0'                                                        
         ZAP   PST,=P'0'                                                        
         LA    R2,BITSAMTH                                                      
         CLI   5(R2),0             POSTING TO SUPPLIER IS OPTIONAL              
         BE    BA9                                                              
         BAS   RE,CASHCHK                                                       
         CLI   ERRNUM,INVAMNT                                                   
         BE    ERROR                                                            
         ZAP   SUPCASH,DUB                                                      
BA9      L     R2,AINCAH                                                        
         BAS   RE,ANY                                                           
         BAS   RE,CASHCHK                                                       
         CLI   ERRNUM,INVAMNT                                                   
         BE    ERROR                                                            
         ZAP   INCCASH,DUB                                                      
         SPACE 1                                                                
         CLI   AGYCTRY,CTRYUSA     TEST FOR US SCREEN                           
         BNE   BA10                NO                                           
         AP    DUB,SUPCASH         CHECK THE ADDITION                           
         CP    DUB,JOBCASH                                                      
         BE    BA10                                                             
         MVI   ERRNUM,INVAMNT                                                   
         LA    R2,BITJAMTH                                                      
         B     ERROR                                                            
         EJECT                                                                  
*              READ AND VALIDATE INPUT ACCOUNTS                                 
         SPACE 2                                                                
BA10     DS    0H                                                               
         LA    R2,BITCLIH          CLIENT                                       
         BAS   RE,ANY                                                           
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         LA    R4,COMPEL                                                        
         USING ACCOMPD,R4                                                       
         MVC   KEY+1(2),ACMPJOB                                                 
         ZIC   R3,BITCLIH+5                                                     
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),BITCLI                                                  
         SPACE 1                                                                
         TM    BITCLIH+4,X'20'                                                  
         BO    BA12                                                             
         NI    BITPROH+4,X'DF'                                                  
         NI    BITJOBH+4,X'DF'                                                  
         MVC   BITCLIN,SPACES                                                   
         OI    BITCLINH+6,X'80'                                                 
         MVC   BITPRON,SPACES                                                   
         OI    BITPRONH+6,X'80'                                                 
         MVC   BITJOBN,SPACES                                                   
         OI    BITJOBNH+6,X'80'                                                 
         XC    CLIPROF,CLIPROF                                                  
         XC    PRODPROF,PRODPROF                                                
         LA    R6,CLIPROF                                                       
         BAS   RE,GETACC                                                        
         MVI   ERRNUM,18                                                        
         TM    ACCTSTAT,X'10'      LOCKED                                       
         BO    ERROR                                                            
         MVC   BITCLIN,ACCTNAME                                                 
         OI    BITCLINH+6,X'80'                                                 
         OI    BITCLIH+4,X'20'                                                  
         SPACE 1                                                                
BA12     DS    0H                                                               
         LA    R6,CLIPROF   IF CLIENT NOT CHANGED,  USE SAVED PROF              
         USING TWAD,RA                                                          
         OC    TWAACCS,TWAACCS     LIMIT ACCESS                                 
         BZ    BA16                                                             
         CLC   TWAACCS,SPACES                                                   
         BE    BA16                                                             
         USING ACPROFD,R6                                                       
         MVI   ERRNUM,55                                                        
         CLI   TWAACCS,C'*'        OFFICE CODE                                  
         BE    BA16                                                             
         CLI   TWAACCS,C'$'        AND LIST WILL BE CHECKED BY BASE             
         BE    BA16                                                             
         CLC   TWAACCS(2),KEY+3    2-CHARACTER CLIENT MATCH                     
         BNE   ERROR                                                            
         SPACE 2                                                                
BA16     MVC   CLINUM,KEY                                                       
         MVC   CLINAM,BITCLIN                                                   
         OC    CLINAM,SPACES                                                    
         LA    R5,KEY+3                                                         
         ZIC   RF,CLILNGTH         LEVA LENGTH                                  
         AR    R5,RF                                                            
         LA    R2,BITPROH          PRODUCT                                      
         BAS   RE,ANY                                                           
         IC    RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),BITPRO                                                   
         LA    R5,KEY+3                                                         
         IC    RF,PRDLNGTH         LEVB LENGTH                                  
         AR    R5,RF                                                            
         TM    BITPROH+4,X'20'                                                  
         BO    BA20                                                             
         XC    PRODPROF,PRODPROF                                                
         NI    BITJOBH+4,X'DF'                                                  
         MVC   BITJOBN,SPACES                                                   
         OI    BITJOBNH+6,X'80'                                                 
         MVC   BITPRON,SPACES                                                   
         OI    BITPRONH+6,X'80'                                                 
         LA    R6,PRODPROF                                                      
         BAS   RE,GETACC                                                        
         MVC   BITPRON,ACCTNAME                                                 
         OI    BITPRONH+6,X'80'                                                 
         OI    BITPROH+4,X'20'                                                  
         SPACE 2                                                                
BA20     MVC   PRODNUM,KEY                                                      
         MVC   PRODNAM,BITPRON                                                  
         OC    PRODNAM,SPACES                                                   
         LA    R6,JOBPROF                                                       
         LA    R2,BITJOBH                                                       
         BAS   RE,ANY              JOB                                          
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),BITJOB                                                   
         XC    JOBPROF,JOBPROF                                                  
         BAS   RE,GETACC                                                        
         MVI   ERRNUM,18                                                        
         TM    ACCTSTAT,X'80'      POSTABLE                                     
         BZ    ERROR                                                            
         TM    ACCTSTAT,X'30'      LOCKED/CLOSED                                
         BNZ   ERROR                                                            
         MVC   BITJOBN,ACCTNAME                                                 
         TM    BITJOBH+4,X'20'                                                  
         BO    BA22                                                             
         OI    BITJOBNH+6,X'80'                                                 
         OI    BITJOBH+4,X'20'                                                  
         SPACE 2                                                                
BA22     MVC   JOBNUM,KEY                                                       
         MVC   JOBNAM,ACCTNAME                                                  
         BAS   RE,PROFMERG                                                      
         LA    R6,PROFILE                                                       
         USING ACPROFD,R6                                                       
         MVC   COSTING,ACPRCOST                                                 
         MVC   FILT,ACPROFFC                                                    
         LA    R6,JOBPROF                                                       
         CLI   ACPROFFC,C' '                                                    
         BNH   *+10                                                             
         MVC   FILT,ACPROFFC                                                    
         OC    FILT,SPACES                                                      
         SPACE 2                                                                
         MVI   COSTBYTE,C'N'                                                    
*&&US                                                                           
         TM    COMPSTAT,X'10'                                                   
         BZ    BA30                                                             
*&&                                                                             
         MVI   COSTBYTE,C'Y'                                                    
         LA    R2,BITCLIH                                                       
         MVC   KEY(15),COSTING     COSTING CLIENT ACCOUNT                       
         BAS   RE,GETACC                                                        
         MVC   COSTNAM,ACCTNAME                                                 
         SPACE 3                                                                
BA30     LA    R2,BITWRKH          WORK-CODE                                    
         TM    BITWRKH+4,X'20'                                                  
         BO    BA35                                                             
         BAS   RE,ANY                                                           
         MVI   ERRNUM,INVALID                                                   
         CLI   5(R2),2             MUST BE 2-LONG                               
         BNE   ERROR                                                            
         CLC   BITWRK,=C'99'                                                    
         BE    ERROR                                                            
         GOTO1 AGETWC,8(R2)                                                     
         BNE   ERROR                                                            
*                                                                               
         MVC   BITWRKN,WORK                                                     
         OI    BITWRKNH+6,X'80'                                                 
         OI    BITWRKH+4,X'20'                                                  
         DROP  R6                                                               
*                                                                               
BA35     MVC   WRKC,8(R2)          SAVE WORKCODE NEAR AMOUNT                    
         LA    R4,JOBNUM                                                        
         LA    R6,WRKC                                                          
         GOTO1 ASETJOB,DMCB,(X'80',(R4)),(R6)                                   
*                                                                               
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    BA38                NO                                           
         LA    R2,BITJOBH                                                       
         MVI   ERRNUM,45                                                        
         B     ERROR                                                            
*                                                                               
BA38     GOTO1 AOPTVAL                                                          
         BNE   ERROR                                                            
*                                                                               
         CLI   CHECK,C' '          CHECKING AMOUNT ?                            
         BE    BA40                NO                                           
         MVI   ERRNUM,30                                                        
         LA    R6,WRKC                                                          
         GOTO1 AWRKVAL,DMCB,(R6)                                                
         BH    ERROR                                                            
         EJECT                                                                  
BA40     SR    R6,R6                                                            
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'09'           GET MEDIA RECORD                             
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(1),BITJOB     MEDIA IS FIRST BYTE OF JOB                   
         BAS   RE,HIGH                                                          
         MVC   FVMSGNO,=Y(AE$MMR)  MISSING MEDIA RECORD'                        
         CLC   KEY(15),KEYSAVE                                                  
         BNE   ERROR               MEDIA RECORD MUST EXIST                      
*                                                                               
         MVI   ELCODE,ACMDELQ      SEARCH FOR MEDIA ELEMENT                     
         L     R4,AIOAREA1                                                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                X'11' ELEMENT MUST EXIST                     
*                                                                               
         USING ACMEDIAD,R4                                                      
         CLI   ACMDLEN,ACMDLNQ2    USING EXPANDED ELEMENT?                      
         BL    BA41                                                             
         MVC   NEWCOST,ACMDCOST     USE 12 BYTE COST ACCOUNT                    
         MVC   DISCACC,ACMDCSHD     SAVE CASH DISCOUNT ACCOUNT                  
*                                                                               
BA41     ZAP   DISC,=P'0'          INIT CASH DISC FIELD                         
         MVC   KEY,SPACES          RESET KEY                                    
         MVC   KEY(1),COMPANY                                                   
         LA    R4,COMPEL                                                        
         USING ACCOMPD,R4                                                       
         MVC   KEY+1(2),ACMPSUPP   SUPPLIER                                     
         LA    R2,BITSUPH                                                       
         CLI   5(R2),0                                                          
         BE    BA42                                                             
         LA    R2,BITSAMTH         IF SUPPLIER-MUST HAVE SUPPLIER CASH          
         BAS   RE,ANY                                                           
         B     BA44                                                             
BA42     CP    SUPCASH,=P'0'                                                    
         BE    BA50                                                             
         BAS   RE,ANY              AND VICE-VERSA                               
BA44     LA    R2,BITSUPH                                                       
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),BITSUP                                                  
*                                                                               
         CLI   BITSUP,C'*'         *SW ETC                                      
         BNE   BA46                                                             
         MVC   FVMSGNO,=Y(AE$INACC)                                             
         CLC   KEY+3(3),=C'*SC'                                                 
         BE    BA45                                                             
         CLC   KEY+3(3),=C'*SW'                                                 
         BE    BA45                                                             
         CLC   KEY+3(3),=C'*SX'                                                 
         BE    BA45                                                             
         CLC   KEY+3(3),=C'*SV'                                                 
         BE    BA45                                                             
         CLC   KEY+3(3),=C'*SY'                                                 
         BNE   ERROR                                                            
*                                                                               
BA45     MVC   WORK(14),KEY+4                                                   
         MVC   KEY+1(17),SPACES                                                 
         MVC   KEY+1(14),WORK                                                   
*                                                                               
BA46     BAS   RE,GETACC             GETS ACTUAL KEY                            
         MVI   ERRNUM,18                                                        
         TM    ACCTSTAT,X'80'      POSTABLE                                     
         BZ    ERROR                                                            
         TM    ACCTSTAT,X'10'      LOCKED                                       
         BO    ERROR                                                            
         MVC   SUPNUM,KEY                                                       
         MVC   SUPNAM,ACCTNAME                                                  
         MVC   BITSUPN,ACCTNAME                                                 
*                                    GET DISCOUNT ELEMENT                       
         BAS   RE,HIGH               READ VENDOR RECORD                         
         CLC   KEY(15),KEYSAVE       STILL SAME VENDOR                          
         BNE   ERROR                                                            
         SR    R3,R3                 FOR IC WITHIN LOOP                         
         LA    R4,IOAREA                                                        
BA47     CLI   0(R4),0               END OF RECORD                              
         BE    BA49                                                             
         CLI   0(R4),X'38'           LOOK FOR DISCOUNT ELEMENT                  
         BE    BA48                                                             
         CLI   0(R4),ITCELQ        TEST FOR INPUT TAX TYPE DEFAULT              
         BE    BA48A               YES                                          
BA47A    IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     BA47                                                             
         SPACE 1                                                                
         USING ACVATD,R4                                                        
BA48     CLI   BITCD,C'N'            DON'T PROCESS C/D                          
         BE    BA47A                 THEN BYPASS THIS                           
         MVC   HALF,ACVTRATE         PACK THE RATE INTO DISC                    
         LH    R1,HALF                                                          
         CVD   R1,DISC                                                          
         DROP  R4                                                               
         SPACE 1                                                                
         CP    DISC,=P'0'            IF CD, PUT THE RATE IN FRONT               
         BE    BA47A                 OF SUPPLIER NAME                           
         EDIT  (P8,DISC),(5,WORK),2                                             
         MVI   WORK+5,C' '                                                      
         MVC   WORK+6(25),ACCTNAME                                              
         GOTO1 SQUASHER,DMCB,WORK,42                                            
         L     R5,DMCB+4             LENGTH FOR CHOPPER                         
         GOTO1 CHOPPER,DMCB,((R5),WORK),(24,BITSUPN),1                          
         B     BA47A                                                            
*                                                                               
         USING ITCELD,R4                                                        
BA48A    CLC   DATE,ITCEFFD        TEST TRANS DATE >= EFFECTIVE DATE            
         BL    BA47A                                                            
         OC    ITCPROV,ITCPROV     NO PROV=GST                                  
         BZ    BA48AG                                                           
         MVC   SUPPSTT,ITCTYPE     SAVE THE PST TYPE                            
         MVC   VENDPROV,ITCPROV                                                 
         B     BA47A                                                            
BA48AG   MVC   SUPTYPE,ITCTYPE                                                  
         B     BA47A                                                            
         DROP  R4                                                               
         SPACE 2                                                                
BA49     OI    BITSUPNH+6,X'80'      SUPPLIER NAME TO SCREEN                    
         OI    BITSUPH+4,X'20'                                                  
         CLI   USERPROV,C'Y'       DID USER ENTERED PROVINCE?                   
         BE    BA50                                                             
         CLI   AGYCTRY,CTRYCAN                                                  
         BNE   BA50                                                             
         OC    VENDPROV,VENDPROV                                                
         BZ    BA50                                                             
         L     R2,APROVH                                                        
         MVI   5(R2),2                                                          
         OI    6(R2),X'80'                                                      
         L     RF,BCAUTL                                                        
         TM    TSTAT6-UTLD(RF),TST6STRO        STEREO?                          
         BZ    BA49X                                                            
         CLC   VENDPROV,SPACES                                                  
         BNH   BA50                                                             
BA49X    MVC   8(2,R2),VENDPROV                                                 
*                                                                               
BA50     CLI   AGYCTRY,CTRYCAN     TEST CANADIAN SCREEN                         
         BNE   BA50X                                                            
*                                                                               
         CLI   PFKEY,7                                                          
         BNE   BA50A                                                            
         CLI   MODE,1                                                           
         BH    BA50A                                                            
         B     BA6                                                              
*                                                                               
BA50A    BAS   RE,EDTAX                                                         
         BE    BA50B                                                            
         CLI   ERRNUM,OK                                                        
         BNE   ERROR                                                            
BA50B    L     RF,=A(CTAXMOD)                                                   
         L     RE,RELO1                                                         
         AR    RF,RE                                                            
         MVI   MODE,4              HAS TO BE 1-PASS                             
         GOTO1 (RF),DMCB,(RC)                                                   
         MVI   FVOMTYP,C'E'                                                     
         CLI   CTXMODE,C'Z'                                                     
         BNE   BA50C                                                            
         MVC   FVMSGNO,CTXMSGNO                                                 
         L     R2,FVADDR                                                        
         B     ERROR                                                            
*                                                                               
BA50C    ZAP   TOTNET,TMPNET                                                    
         ZAP   TOTGRS,TMPGRS                                                    
         ZAP   GST,TMPGST                                                       
         ZAP   PST,TMPPST                                                       
*                                                                               
         ZAP   DUB,TOTGRS          CHECK ARITHMETIC                             
         AP    DUB,INCCASH         DUB=CREDITS                                  
         ZAP   DUB1,JOBCASH        DUB1=DEBITS                                  
         AP    DUB1,GST                                                         
         AP    DUB1,PST                                                         
         CP    DUB,DUB1            TEST DEBITS=CREDITS                          
         BNE   BA50D               YES                                          
         ZAP   PL16,DISC                                                        
         MP    PL16,TOTNET                                                      
         ZAP   DISC,PL16                                                        
         SRP   DISC,64-4,5         CD RATE IS A PERCENT W/2 DEC PLACES          
         B     BA51                                                             
*                                                                               
BA50D    LA    R2,BITJAMTH                                                      
         MVC   FVMSGNO,=AL2(FVFOK) ENSURE CORRECT NUMBER                        
         MVI   ERRNUM,INVAMNT                                                   
         B     ERROR                                                            
*                                                                               
BA50X    ZAP   TOTGRS,SUPCASH      NON-CANADIAN                                 
         ZAP   PL16,DISC                                                        
         MP    PL16,TOTGRS                                                      
         ZAP   DISC,PL16                                                        
         SRP   DISC,64-4,5         CD RATE IS A PERCENT W/2 DEC PLACES          
*                                                                               
BA51     MVC   KEY+3(12),SPACES    INCOME ACCOUNT                               
         MVC   KEY+1(2),=C'SI'                                                  
         L     R2,AINCH                                                         
         BAS   RE,ANY                                                           
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),8(R2)                                                   
         CLI   KEY+3,C'*'                                                       
         BNE   BA52                                                             
         MVC   WORK(14),KEY+4                                                   
         MVC   KEY+1(17),SPACES                                                 
         MVC   KEY+1(14),WORK                                                   
*                                                                               
         CLC   KEY+1(2),=C'SB'                                                  
         BE    BA51A                                                            
         CLC   KEY+1(2),=C'SC'                                                  
         BE    BA51A                                                            
         CLC   KEY+1(2),=C'SK'                                                  
         BNE   BA51B                                                            
*                                                                               
BA51A    CP    TOTGRS,=P'0'        IF NO SUPPLIER POSTING THEN                  
         BE    BA52                IT'S OK TO POST TO SK                        
         CLC   KEY+1(2),=C'SK'                                                  
         BNE   BA52                                                             
*                                                                               
BA51B    XC    FVMSGNO,FVMSGNO     CLEAR MESSAGE                                
         MVI   ERRNUM,17                                                        
         B     ERROR                                                            
*                                                                               
BA52     BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVI   ERRNUM,18                                                        
         TM    ACCTSTAT,X'80'                                                   
         BZ    ERROR                                                            
         TM    ACCTSTAT,X'10'                                                   
         BO    ERROR                                                            
         MVC   INCNUM,KEY                                                       
         MVC   INCNAM,ACCTNAME                                                  
         CLI   ACCTCOST,C' '       IS THERE A COSTING CODE                      
         BNH   *+16                                                             
         MVC   NEWCOST,SPACES                                                   
         MVC   NEWCOST(1),ACCTCOST REPLACE CODE FROM MEDIA                      
         TM    4(R2),X'20'                                                      
         BO    BA54                                                             
*                                                                               
         OI    4(R2),X'20'                                                      
         L     R2,AINCNH                                                        
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'ACCTNAME),ACCTNAME                                         
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
BA54     CLI   COSTBYTE,C'Y'                                                    
         BNE   BA60                                                             
         TM    COMPSTAT,X'10'      COSTING REQUIRED                             
         BZ    BA60                                                             
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),INCNUM                                                   
         BAS   RE,READ                                                          
         L     R4,AIOAREA          INCOME ACCOUNT IN IOAREA                     
         MVI   ELCODE,ACSTELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   BA55                GET STATUS ELEMENT                           
*                                                                               
         USING ACSTATD,R4                                                       
         OC    ACSTCNTR,SPACES                                                  
         CLI   ACSTCPOS,0          IF 0 START AT AT KEY+7                       
         BE    *+20                                                             
         LA    RE,COSTING+2        ADDR OF KEY+2                                
         ZIC   R0,ACSTCPOS         STARTING POINT NUMBER                        
         AR    RE,R0               NEW STARTING POINT                           
         B     *+8                                                              
         LA    RE,COSTING+7        CLIENT COSTING KEY+7                         
         LA    R3,3                                                             
         LA    RF,ACSTCNTR                                                      
*                                                                               
         CLI   0(RF),C' '          REPLACE ONLY NON-BLANK                       
         BE    *+10                                                             
         MVC   0(1,RE),0(RF)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R3,*-22                                                          
*                                                                               
BA55     L     R4,AIOAREA          INCOME ACCOUNT IN IOAREA                     
         MVI   ELCODE,ACSPELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   BA58                GET STATUS ELEMENT                           
*                                                                               
         USING ACSPECD,R4                                                       
BA56     CLI   ACSPTYP,ACSPOAN     COST OVERRIDE                                
         BE    BA57                                                             
         BAS   RE,NEXTEL                                                        
         BE    BA56                                                             
         B     BA58                                                             
*                                                                               
BA57     MVC   NEWCOST,ACSPACCT    SAVE NEW COSTING CODE                        
*                                                                               
BA58     CLC   INCNUM+1(2),=C'SK' NO POINTER IN ACCSTAT+1 FOR OVERRIDES         
         BE    BA60                                                             
         CLC   INCNUM+1(2),=C'SB'                                               
         BE    BA60                                                             
         CLC   INCNUM+1(2),=C'SC'                                               
         BE    BA60                                                             
*                                                                               
         MVC   KEY+3(12),SPACES                                                 
         MVC   KEY+1(2),=C'12'     HARD FOR COSTING COMMISSIONS                 
         MVC   KEY+3(12),NEWCOST   NEW COSTING ACCOUNT                          
         BAS   RE,GETACC                                                        
         TM    ACCTSTAT,X'80'                                                   
         BZ    ERROR                                                            
         TM    ACCTSTAT,X'10'                                                   
         BO    ERROR                                                            
         MVC   CINCNUM,KEY                                                      
         MVC   CINCNAM,ACCTNAME                                                 
*                                                                               
BA60     DS    0H                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),COSTING                                                  
         LA    R2,BITCLIH                                                       
         USING ACPROFD,RF                                                       
         LA    RF,PROFILE                                                       
         CLC   COSTING,ACPRCOST                                                 
         BE    *+8                                                              
         LA    R2,BITINCH                                                       
         DROP  RF                                                               
         BAS   RE,GETACC           VALIDATE MANIPULATED COSTING                 
         BAS   RE,CHECKACC                                                      
         MVC   COSTNAM,ACCTNAME                                                 
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'PRODNUM),PRODNUM            LOOK FOR SALES ELEMENT         
         BAS   RE,HIGH                                                          
         LA    R1,IOAREA                                                        
         SR    RF,RF                                                            
BA62     CLI   0(R1),0                                                          
         BE    BA70                                                             
         CLI   0(R1),X'3D'                                                      
         BE    BA64                                                             
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     BA62                                                             
         USING ACSAND,R1                                                        
BA64     MVC   PRODNUM,ACSACODE                                                 
         MVC   PRODNAM,ACSANAME                                                 
         SPACE 2                                                                
*              FIND OUT WHAT TO DO WITH THE CASH DISCOUNT                       
BA70     DS    0H                                                               
         MVI   SIMDSW,C'N'                                                      
         CP    DISC,=P'0'            IS THERE A DISCOUNT                        
         BE    BA80                  NO JUST POST                               
*                                                                               
         USING GOBLOCKD,R4                                                      
         L     R4,AGOBLOCK                                                      
         CLI   COCDPASS,C'N'         COMPANY SAYS DONT PASS C/D                 
         BE    *+12                                                             
         CLI   GOCLICD,C'N'          CLIENT SAYS DONT PASS CD                   
         BNE   BA80                  ELSE, APPLY CD TO VENDOR                   
*                                                                               
         CLI   DISCACC,C' '        CASH DISCOUNT FROM MEDIA RECORD              
         BH    BA77                ALREADY GOT ACCOUNT                          
         LA    R4,COMPEL                                                        
         USING ACCOMPD,R4                                                       
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),ACMPJOB    GET LEDGER RECORD                            
         BAS   RE,READ                                                          
         L     R4,AIOAREA                                                       
         MVI   ELCODE,ACLTELQ      GET LEDGER ELEMENT                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACLEDGD,R4                                                       
         CLI   ACLTCDAC,C' '       NO LEDGER DEFAULT                            
         BNH   BA74                                                             
         MVC   DISCACC(1),COMPANY                                               
         MVC   DISCACC+1(14),ACLTCDAC                                           
         B     BA77                                                             
*                                                                               
BA74     MVC   DISCACC,SPACES                                                   
         MVC   DISCACC(1),COMPANY                                               
         MVC   DISCACC+1(4),=C'SIMD'                                            
*                                                                               
BA77     MVC   KEY,SPACES                                                       
         MVC   KEY(15),DISCACC       GET CD ACCOUNT                             
         BAS   RE,GETACC             GET SIMD ACCOUNT NAME                      
         MVC   SIMDAC,ACCTNUM                                                   
         MVC   SIMDACN,ACCTNAME                                                 
         MVI   SIMDSW,C'Y'                                                      
         EJECT                                                                  
*              NOW DO SOME POSTING                                              
         SPACE 2                                                                
BA80     DS    0H                                                               
         LA    R7,IOAREA+2                                                      
         USING DLDESCD,R7                                                       
         MVC   DLDSREF,DOCSAVE                                                  
         MVC   DLDSDATE,DATE                                                    
         MVI   DLDSEL,DLDSELQ                                                   
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
         L     R2,ANARH                                                         
         LA    R3,DLDSNARR                                                      
         XC    DLDSNARR,DLDSNARR                                                
         BAS   RE,NARRSCAN                                                      
         SR    R3,R7                                                            
         AR    R3,R6                                                            
         STC   R3,DLDSLEN                                                       
         SPACE 2                                                                
         AR    R7,R3                                                            
         CP    TOTGRS,=P'0'                                                     
         BE    BA82                                                             
         USING ACOTHERD,R7                                                      
         MVC   ACOTEL(2),=X'230F'  OTHERS ELEMENT                               
         MVC   ACOTNUM(13),SPACES                                               
         ZIC   R3,BITPROH+5                                                     
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ACOTNUM(0),BITPRO                                                
         IC    R3,BITJOBH+5                                                     
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ACOTNUM+6(0),BITJOB                                              
         IC    R3,ACOTLEN                                                       
         AR    R7,R3                                                            
         SPACE 1                                                                
         CP    DISC,=P'0'            IS THERE A CASH DISC                       
         BE    BA80A                 NO                                         
         SPACE 1                                                                
         USING TRCASHD,R7            CREATE A MEMO FOR THE C/D                  
         MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'D'         D FOR CASH DISC                            
         ZAP   TRCSAMNT,DISC         AMNONT OF CD                               
         ZIC   R3,TRCSLEN                                                       
         AR    R7,R3                                                            
*                                                                               
BA80A    CLI   AGYCTRY,CTRYCAN     TEST CANADA                                  
         BNE   BA80B                                                            
         CLI   GSTSW,C'Y'          TEST GST APPLICABLE                          
         BNE   BA80B                                                            
*                                                                               
         USING SCIELD,R7                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN3Q                                                    
         MVI   SCITYPE,SCITTAXP    TAXES PAID                                   
         ZAP   SCIAMNT,GST                                                      
         ZAP   SCIBASE,TOTNET                                                   
         MVC   SCISUBPR,SPACES                                                  
         MVC   SCISUBPT,BICTYPE                                                 
         ZIC   R3,SCILN                                                         
         AR    R7,R3                                                            
*                                                                               
         USING SCIELD,R7                                                        
BA80A2   L     RF,APROVH                                                        
         CLI   5(RF),0             ANY PROVINCE?                                
         BZ    BA80B                                                            
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN3Q                                                    
         MVI   SCITYPE,SCITTQST    PST                                          
         ZAP   SCIAMNT,PST                                                      
         ZAP   SCIBASE,TOTNET                                                   
         AP    SCIBASE,GST                                                      
         MVC   SCISUBPR,8(RF)                                                   
         LA    RE,XTRAELM                                                       
         MVC   SCISUBPT,3(RE)                                                   
         ZIC   R3,SCILN                                                         
         AR    R7,R3                                                            
*                                                                               
         USING DLPOSTD,R7                                                       
BA80B    ST    R7,FULL                                                          
         MVI   DLPSEL,DLPSECRQ     CREDIT SUPPLIER                              
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,CLINUM       DEBIT THE CLIENT                           
         MVC   DLPSDBNM,CLINAM                                                  
         MVC   DLPSCRAC,SUPNUM       CREDIT THE SUPPLIER                        
         MVC   DLPSCRNM,SUPNAM                                                  
         ZAP   DLPSAMNT,TOTGRS                                                  
         CP    DISC,=P'0'           IS THERE A C/D                              
         BE    BA81                                                             
         SP    DLPSAMNT,DISC        SUBTRACT IT FROM THE POSTING                
         SPACE 1                                                                
BA81     MVC   DLPSANAL,FILT        FINISH UP THE CREDIT                        
         MVI   DLPSTYPE,0                                                       
         ZIC   R3,DLPSLEN                                                       
         AR    R7,R3                                                            
         SPACE 1                                                                
         USING DLPOSTD,R7                                                       
         CLI   SIMDSW,C'Y'           POST CD TO SIMD OPTION                     
         BNE   BA82                  NO                                         
         L     R6,FULL               ADDRESS OF PREVIOUS 6A                     
         MVC   DLPOSTD(DLPSLNQ),0(R6)                                           
         MVC   DLPSCRAC,SIMDAC       CREDIT SIMD                                
         MVC   DLPSCRNM,SIMDACN                                                 
         MVC   DLPSDBAC,PRODNUM      DEBIT PRODUCT                              
         MVC   DLPSDBNM,PRODNAM                                                 
         ZAP   DLPSAMNT,DISC                                                    
         ZIC   R3,DLPSLEN                                                       
         AR    R7,R3                                                            
         SPACE 1                                                                
         USING TRCASHD,R7                                                       
BA82     DS    0H                                                               
*&&US                                                                           
         CLC   INCNUM+1(2),=C'SK'  NO MEMO ON OVERRIDES                         
         BE    BA84                                                             
         CLC   INCNUM+1(2),=C'SB'                                               
         BE    BA84                                                             
         CLC   INCNUM+1(2),=C'SC'                                               
         BE    BA84                                                             
         MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'G'                                                    
         ZAP   TRCSAMNT,=P'0'      GROSS MEMO OF ZERO ON SI                     
         ZIC   RF,TRCSLEN                                                       
         AR    R7,RF                                                            
*                                                                               
         USING ACMTD,R7                                                         
         XC    0(ACMTLNQ,R7),0(R7)                                              
         MVI   ACMTEL,ACMTELQ      ADD MEDIA TRANSFER FOR TO SI POSTING         
         MVI   ACMTLEN,ACMTLNQ                                                  
         MVI   ACMTSYS,C'J'                                                     
         MVC   ACMTMED,JOBNUM+9      MEDIA CODE                                 
         MVC   ACMTCLI(12),JOBNUM+3  CLI/PRD/JOB                                
         MVC   ACMTMOS,PMOS                                                     
         MVC   ACMTDSCP,JOBNAM                                                  
         OC    ACMTDSCP,SPACES                                                  
         ZAP   DUB,INCCASH                                                      
         CVB   R0,DUB                                                           
         STCM  R0,15,ACMTCOM         INCOME                                     
         ZIC   RF,ACMTLEN                                                       
         AR    R7,RF                                                            
*                                                                               
*&&                                                                             
         SPACE 1                                                                
         USING DLPOSTD,R7          CREDIT INCOME                                
BA84     DS    0H                                                               
         MVI   DLPSEL,DLPSECRQ     CREDIT INCOME                                
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,PRODNUM                                                 
         MVC   DLPSDBNM,PRODNAM                                                 
         MVC   DLPSCRAC,INCNUM                                                  
         MVC   DLPSCRNM,INCNAM                                                  
         CLC   INCNUM+1(2),=C'SK'                                               
         BNE   *+16                                                             
         MVC   DLPSDBAC,JOBNUM                                                  
         MVC   DLPSDBNM,JOBNAM                                                  
         ZAP   DLPSAMNT,INCCASH                                                 
         MVC   DLPSANAL,FILT                                                    
         MVI   DLPSTYPE,0                                                       
         ZIC   RF,DLPSLEN                                                       
         AR    R7,RF                                                            
         SPACE 1                                                                
BA85     CLI   AGYCTRY,CTRYCAN     TEST CANADIAN SCREEN                         
         BNE   BA86                                                             
         CLI   GSTSW,C'Y'          TEST GST APPLIES                             
         BNE   BA86                                                             
         L     R2,AIOA                                                          
         ZICM  RF,0(R2),2                                                       
         LTR   RF,RF                                                            
         BZ    BA86                                                             
         SH    RF,=H'2'                                                         
*                                  DONT POST LAST GST/PST ELEMENT               
         L     R1,ATYPEH                                                        
         CLI   8(R1),C'*'          EXCLUDE GST                                  
         BE    *+10                                                             
         LA    R1,SCILN3Q          LENGTH OF 50 ELEMENT                         
         SR    RF,R1               DELETE 1 50 ELEMENT FOR GST                  
         L     R1,APROVH           DELETE 1 MORE 50 FOR PST                     
         CLI   5(R1),0                                                          
         BZ    *+10                                                             
         LA    R1,SCILN3Q                                                       
         SR    RF,R1                                                            
*                                                                               
         LA    RE,2(R2)                                                         
         LR    R0,R7                                                            
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         LR    R7,R0                                                            
*                                                                               
         USING TRCASHD,R7                                                       
BA86     MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'C'                                                    
         ZAP   TRCSAMNT,INCCASH    INCOME MEMO ON JOB                           
         ZIC   RF,TRCSLEN                                                       
         AR    R7,RF                                                            
*                                                                               
         CP    DISC,=P'0'            ANY C/D?                                   
         BE    BA88                  NO, NO CD MEMO                             
         CLI   SIMDSW,C'Y'           PASSED  TO SIMD                            
         BE    BA88                  YES, NO CD MEMO                            
         USING TRCASHD,R7                                                       
         MVI   TRCSEL,TRCSELQ      POSTING MEMO FOR JOB                         
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'D'                                                    
         ZAP   TRCSAMNT,DISC                                                    
         ZIC   R3,TRCSLEN                                                       
         AR    R7,R3                                                            
*                                                                               
         USING TRSDESCD,R7                                                      
BA88     MVI   TRSDEL,TRSDELQ                                                   
         MVC   TRSDLEN,=YL1(L'ACKEYACC-1+2)                                     
         MVC   TRSDACCS(L'ACKEYACC-1),INCNUM+1                                  
         ZIC   RF,TRSDLEN                                                       
         AR    R7,RF                                                            
*                                                                               
         CP    TOTGRS,=P'0'        ANY SUPPLIER CASH?                           
         BE    BA88B               NO, SKIP PAYABLE ELEMENT THEN                
         CLC   SUPNUM+1(2),=C'SV'                                               
         BE    BA88A                                                            
         CLC   SUPNUM+1(2),=C'SW'                                               
         BE    BA88A                                                            
         CLC   SUPNUM+1(2),=C'SX'                                               
         BE    BA88A                                                            
         CLC   SUPNUM+1(2),=C'SY'                                               
         BNE   BA88B                                                            
*                                                                               
         USING PAKELD,R7                                                        
BA88A    MVI   PAKEL,PAKELQ                                                     
         MVI   PAKLN,PAKLNQ                                                     
         MVC   PAKACC,SUPNUM                                                    
         MVC   PAKOFF,FILT                                                      
         MVC   PAKCON,CLINUM                                                    
         MVC   PAKDATE,DATE                                                     
         MVC   PAKREF,DOCSAVE                                                   
         ZIC   RF,PAKLN                                                         
         AR    R7,RF                                                            
*                                                                               
         USING DLPOSTD,R7                                                       
BA88B    MVI   DLPSEL,DLPSEDRQ     DEBIT JOB                                    
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,JOBNUM                                                  
         MVC   DLPSDBNM,JOBNAM                                                  
         CP    TOTGRS,=P'0'        IF THERE IS NO SUPPLIER CASH                 
         BNE   BA89                THEN                                         
         MVC   DLPSCRAC,INCNUM     CREDIT INCOME                                
         MVC   DLPSCRNM,INCNAM                                                  
         ZAP   DLPSAMNT,JOBCASH                                                 
         B     BA90                 (SKIP THE CASH DISC STUFF)                  
BA89     MVC   DLPSCRAC,SUPNUM     ELSE CREDIT THE SUPPLIER                     
         MVC   DLPSCRNM,SUPNAM                                                  
         ZAP   DLPSAMNT,JOBCASH                                                 
         SPACE 1                                                                
         CP    DISC,=P'0'            ANY C/D?                                   
         BE    BA90                  NO, POST FULL                              
         CLI   SIMDSW,C'Y'           CD POSTED TO SIDM                          
         BE    BA90                  YES, POST IN FULL                          
         SP    DLPSAMNT,DISC                                                    
BA90     MVC   DLPSANAL,BITWRK                                                  
         MVI   DLPSTYPE,0                                                       
         IC    RF,DLPSLEN                                                       
         AR    R7,RF                                                            
         CLI   COSTBYTE,C'N'       COSTING                                      
         BE    BA100                                                            
*&&US                                                                           
         CLC   INCNUM+1(2),=C'SK'  NO COSTING ON SK POSTING                     
         BE    BA100                                                            
         CLC   INCNUM+1(2),=C'SB'  NO COSTING ON SB POSTING                     
         BE    BA100                                                            
         CLC   INCNUM+1(2),=C'SC'  NO COSTING ON SC POSTING                     
         BE    BA100                                                            
*&&                                                                             
         MVI   DLPSEL,DLPSEDCQ     DEBIT/CREDIT                                 
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,COSTING                                                 
         MVC   DLPSDBNM,COSTNAM                                                 
         MVC   DLPSCRAC,CINCNUM                                                 
         MVC   DLPSCRNM,CINCNAM                                                 
         ZAP   DLPSAMNT,INCCASH                                                 
         MVC   DLPSANAL,FILT                                                    
         MVI   DLPSTYPE,X'80'                                                   
         IC    RF,DLPSLEN                                                       
         AR    R7,RF                                                            
         SPACE 1                                                                
BA100    MVI   0(R7),0                                                          
         LA    R6,IOAREA-1                                                      
         SR    R7,R6                                                            
         STH   R7,HALF                                                          
         MVC   IOAREA(2),HALF                                                   
         BAS   RE,PUTDAY                                                        
         XC    WORK(20),WORK                                                    
         L     RF,DMCB+8                                                        
         MVC   WORK+10(4),0(RF)    DISK ADDRESS                                 
         ZIC   RF,BITDOCH+5                                                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),BITDOC                                                   
         ZAP   TRANSAMT,JOBCASH                                                 
         AP    TRANSAMT,GST                                                     
         AP    TRANSAMT,PST                                                     
         ZAP   DUB,TRANSAMT                                                     
         BAS   RE,ADSCRINF                                                      
*                                                                               
         MVI   GSTSW,C'N'          GST/PST NOT APPLICABLE                       
         MVI   CTAXBEF,C'N'        NEVER USED CTAX                              
         XC    KEEPPROV,KEEPPROV                                                
         MVI   CLEAROK,C'Y'        CLEAR GST/PST NEXT TIME                      
         MVI   USERPROV,C'N'                                                    
         MVI   USERPST,C'N'                                                     
         CLI   AGYCTRY,CTRYCAN     CANADIAN?                                    
         BNE   *+12                                                             
         L     R2,APROVH                                                        
         MVI   4(R2),0                                                          
         LA    R2,BITDOCH                                                       
         MVI   ERRNUM,X'FF'                                                     
         B     EXIT                                                             
         EJECT                                                                  
*--------------------------------------------------------------                 
*        SUB-ROUTINE TO EDIT TAX FIELDS AND DISPLAY DATA                        
*        ON EXIT, CC=EQ IF OK, NEQ IF ERROR AND R2 SET TO ERR FLD               
*--------------------------------------------------------------                 
EDTAX    NTR1                                                                   
         MVI   GSTSW,C'N'                                                       
         MVI   ERRNUM,OK                                                        
         MVI   MSGNO,X'00'                                                      
         L     R2,ATYPEH                                                        
         CLI   8(R2),C'*'          GST/PST NOT APPLICABLE?                      
         BE    EDTAXX                                                           
*                                                                               
         LA    R2,BITSUPH                                                       
         MVI   ERRNUM,1            GST/PST NEEDS A SUPPLIER                     
         CLI   5(R2),0                                                          
         BE    EDTAXX                                                           
*                                                                               
         L     R2,AGORNH           POINT TO GROSS/NET                           
         MVI   ERRNUM,1            MISSING INPUT                                
         CLI   5(R2),0                                                          
         BE    EDTAXX                                                           
*                                                                               
         MVC   GORN,8(R2)                                                       
         MVI   ERRNUM,2                                                         
         CLI   GORN,C'G'                                                        
         BE    *+12                                                             
         CLI   GORN,C'N'                                                        
         BNE   EDTAXX                                                           
*                                                                               
         MVI   CTXMODE,C'G'                                                     
         MVI   ERRNUM,OK                                                        
         CLI   CTAXBEF,C'Y'        HAVE TO HAVE USE CTAX SCREEN                 
         BNE   EDTAXX                                                           
*                                                                               
EDTAX50  ZAP   TOTNET,=P'0'                                                     
         ZAP   TOTGRS,=P'0'                                                     
         ZAP   GST,=P'0'                                                        
         ZAP   PST,=P'0'                                                        
*                                                                               
EDTAXX   CLI   ERRNUM,OK           YES-RESET VENDOR AMT=GROSS                   
XITR2    XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*---------------------------------------------------------------                
* ADSCRINF - POST & SAVE 2ND SCREEN'S INFO                                      
*---------------------------------------------------------------                
ADSCRINF NTR1  ,                                                                
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         LA    RF,XTRAELM                                                       
         GOTO1 AADACDAY,BOPARM,(X'80',IOAREA),BOPL61,BOWORK1,(RF)               
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   ERRXIT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*              CHECK CASH FIELDS                                                
         SPACE 2                                                                
CASHCHK  NTR1                                                                   
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(X'82',8(R2)),(R3)                                  
         CLI   0(R1),X'FF'                                                      
         BNE   CASH2                                                            
         MVI   ERRNUM,INVAMNT                                                   
         B     CASHXT                                                           
         SPACE 1                                                                
CASH2    ZAP   DUB,4(8,R1)                                                      
CASHXT   XIT1                                                                   
         SPACE 2                                                                
* SUB-ROUTINE TO MOVE OUT DISPLAY DATA                                          
*                                                                               
MOVEFLD  ST    RE,SAVERE                                                        
         ZIC   R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),FLD                                                      
*                                                                               
MOVEFLDX L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
* US SCREEN TABLE  ( BYTES 0-3=DISP TO FIELD, BYTES 4-7=DISP TO ADCON )         
*                                                                               
USTAB    DS    0D                                                               
         DC    AL4(BITINCH-TWAD),AL4(AINCH-PROGD)                               
         DC    AL4(BITINCNH-TWAD),AL4(AINCNH-PROGD)                             
         DC    AL4(BITIAMTH-TWAD),AL4(AINCAH-PROGD)                             
         DC    AL4(BITNARH-TWAD),AL4(ANARH-PROGD)                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* CA SCREEN TABLE  ( BYTES 0-3=DISP TO FIELD, BYTES 4-7=DISP TO ADCON )         
*                                                                               
CANTAB   DS    0D                                                               
         DC    AL4(BICTYPEH-TWAD),AL4(ATYPEH-PROGD)                             
         DC    AL4(BICTYPNH-TWAD),AL4(ATYPNH-PROGD)                             
         DC    AL4(BICGORNH-TWAD),AL4(AGORNH-PROGD)                             
         DC    AL4(BICGSTXH-TWAD),AL4(AGSTXH-PROGD)                             
         DC    AL4(BICGAMTH-TWAD),AL4(AGAMTH-PROGD)                             
         DC    AL4(BICPROVH-TWAD),AL4(APROVH-PROGD)                             
         DC    AL4(BICINCH-TWAD),AL4(AINCH-PROGD)                               
         DC    AL4(BICINCNH-TWAD),AL4(AINCNH-PROGD)                             
         DC    AL4(BICIAMTH-TWAD),AL4(AINCAH-PROGD)                             
         DC    AL4(BICNARH-TWAD),AL4(ANARH-PROGD)                               
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE ACBATCODE                                                      
         EJECT                                                                  
NOCTAX   DC    C'** ERROR - CANADIAN SCREEN NOT AVAILABLE'                      
         LTORG                                                                  
         EJECT                                                                  
CTAXMOD  DS    0D                                                               
         NMOD1 0,**CTAX**                                                       
         L     RC,0(R1)                                                         
         USING PROGD,RC                                                         
         USING TWAD,RA                                                          
         USING GWS,R9                                                           
         MVI   CTXMODE,C'E'                                                     
         CLI   MODE,3                                                           
         BE    CTAX50                                                           
         MVI   CTXMODE,C'B'                                                     
         CLI   MODE,4                                                           
         BNE   *+8                                                              
         MVI   CTXMODE,C'G'        GO THROUGH                                   
*                                                                               
         CLI   CSACT,ACTCHA                                                     
         BNE   CTAX10                                                           
         CLI   CTAXBEF,C'Y'        DID WE READ BEFORE?                          
         BE    *+8                 YES, DON'T RESET AGAIN                       
         BAS   RE,EXTRAELM                                                      
CTAX10   MVC   CTXXTELM,XTRAELM                                                 
*                                                                               
         MVC   AMTBLK(2),=C'TT'    NEED WORK CODE FOR BT41                      
         ZAP   AMTBLK+2(6),SUPCASH 1 AMOUNT                                     
         LA    RF,7                                                             
         LA    RE,AMTBLK+8         CLEAR THE REST                               
CTAX13   MVC   0(2,RE),SPACES                                                   
         ZAP   2(6,RE),=P'0'                                                    
         LA    RE,8(RE)                                                         
         BCT   RF,CTAX13                                                        
*                                                                               
         MVC   CTXACC(1),COMPANY                                                
         MVC   CTXOFF,FILT                                                      
         L     R2,AGORNH                                                        
         MVC   CTXGORN,8(R2)                                                    
         L     R2,APROVH                                                        
         MVC   CTXPROV,8(R2)                                                    
         MVC   CTXDATE,DATE                                                     
*        MVC   CTXACC,JOBNUM                                                    
         MVC   CTXVENGT,SUPTYPE                                                 
         MVC   CTXVENPT,SUPPSTT                                                 
         L     R2,ATYPEH                                                        
         MVC   CTXGSTT,8(R2)                                                    
         L     R2,ATYPNH                                                        
         MVC   CTXGSTTN,8(R2)                                                   
         L     R2,AGAMTH                                                        
         MVC   CTXLGSTA,5(R2)                                                   
         MVC   CTXGSTA,8(R2)                                                    
         MVC   CTXUPSTT,USERPST                                                 
         MVC   CTXUPROV,USERPROV                                                
*                                                                               
CTAX15   MVC   CTXCNTRA,SUPNUM     VENDOR                                       
         MVC   CTXCNTRN,SUPNAM                                                  
*                                                                               
CTAX50   LA    R3,X'41'            CTAX SCREEN                                  
         GOTO1 CALLOV,DMCB,((R3),0),(0,0)                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,CTXDATA,(R9),AMTBLK                                    
         MVI   MODE,3                                                           
         CLI   CTXMODE,C'E'                                                     
         BE    CTAXXIT                                                          
         CLI   CTXMODE,C'G'        GOOD 1-PASS                                  
         BE    CTAX60                                                           
         CLI   CTXMODE,C'Z'        ERROR ON 1-PASS                              
         BE    CTAX60                                                           
         CLI   CTXMODE,C'X'                                                     
         BNE   CTAXXIT                                                          
CTAX60   MVI   MODE,0                                                           
         CLI   CTXMODE,C'Z'                                                     
         BE    *+8                                                              
         MVI   CTAXBEF,C'Y'        USED CTAX BEFORE                             
*                                                                               
         MVC   GSTSW,CTXAPPL       SEE IF GST/PST APPLICABLE                    
*                                                                               
         MVC   TMPNET,CTXNET                                                    
         MVC   TMPGST,CTXGST                                                    
         MVC   TMPPST,CTXPST                                                    
         MVC   TMPGRS,CTXGRS                                                    
*                                                                               
         MVC   USERPST,CTXUPSTT                                                 
         CLI   CTXUPROV,C'Y'                                                    
         BNE   CTAX61                                                           
         MVI   USERPROV,C'Y'                                                    
         MVC   KEEPPROV,CTXPROV                                                 
*                                                                               
CTAX61   L     R2,AGORNH           SHOW CHANGES                                 
         MVC   8(1,R2),CTXGORN                                                  
         OI    6(R2),X'80'                                                      
         L     R2,APROVH                                                        
         MVC   8(2,R2),CTXPROV                                                  
         OI    6(R2),X'80'                                                      
         L     R2,ATYPEH                                                        
         MVC   8(1,R2),CTXGSTT                                                  
         OI    6(R2),X'80'                                                      
         L     R2,ATYPNH                                                        
         MVC   8(21,R2),CTXGSTTN                                                
         OI    6(R2),X'80'                                                      
         L     R2,AGAMTH                                                        
         MVC   8(10,R2),SPACES                                                  
         MVC   5(1,R2),CTXLGSTA                                                 
         CLI   CTXLGSTA,0                                                       
         BZ    CTAX65                                                           
         ZIC   R1,CTXLGSTA                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),CTXGSTA                                                  
CTAX65   OI    6(R2),X'80'                                                      
*                                                                               
         L     R2,AGSTXH                                                        
         XC    8(L'BICGSTX,R2),8(R2)                                            
         CLI   GSTSW,C'Y'                                                       
         BNE   CTAX69                                                           
         OC    CTXGST,CTXGST       DO WE HAVE GST?                              
         BZ    CTAX70                                                           
         MVC   8(4,R2),=C'GST='                                                 
         EDIT  CTXGST,(14,12(R2)),2,ZERO=NOBLANK,ALIGN=LEFT                     
CTAX69   OI    6(R2),X'80'                                                      
*                                                                               
CTAX70   MVC   XTRAELM,CTXXTELM                                                 
         L     R2,ATYPEH           PUT A * IN AMT WHEN WE RETURN                
         ST    R2,FADR                                                          
         CLI   CTXMODE,C'G'        EXCEPT FOR 1 PASS                            
         BE    CTAXXIT                                                          
         OI    6(R2),X'01'         MODIFIED FIELD, TO HIT ENTER                 
*                                                                               
         CLI   CTXMODE,C'Z'                                                     
         BNE   CTAXXIT                                                          
         L     R2,AGORNH                                                        
         CLI   CTXERR,1                                                         
         BNE   *+8                                                              
         L     R2,ATYPEH                                                        
         CLI   CTXERR,2                                                         
         BNE   *+8                                                              
         L     R2,AGAMTH                                                        
         CLI   CTXERR,3                                                         
         BNE   *+8                                                              
         L     R2,APROVH                                                        
         ST    R2,FVADDR                                                        
*                                                                               
CTAXXIT  XMOD1                                                                  
         EJECT                                                                  
*------------------------------------------------------------------             
* EXTRAELM - EXTRACTS THE SPECIAL 2ND SCREEN FROM ITEM RECORD                   
*------------------------------------------------------------------             
EXTRAELM NTR1                                                                   
         LA    R2,BCITECUR         HAVE TO READ ITEM RECORD                     
         USING LSTTABD,R2                                                       
         MVC   IODAOVER,LSTTDA                                                  
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO1                                                          
         USING TBARECD,R2                                                       
         LA    R2,TBARFST          DISP TO 1ST ELEMENT                          
         USING SFSELD,R2                                                        
EXTRA10  CLI   SFSEL,0             END OF ELEMENTS                              
         BE    EXTRAXIT                                                         
         CLI   SFSEL,SFSELQ        SCREEN FIELD SAVE ELEMENT                    
         BNE   EXTRA50                                                          
         CLI   SFSFLDN,128         1ST FIELD # SAVED                            
         BNE   EXTRA50                                                          
         MVC   XTRAELM,0(R2)       COPY CHUNK                                   
         B     EXTRAXIT                                                         
*                                                                               
EXTRA50  ZIC   R0,SFSLN                                                         
         AR    R2,R0                                                            
         B     EXTRA10                                                          
EXTRAXIT XIT1                                                                   
*                                                                               
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE ACBATDSECT                                                     
       ++INCLUDE ACBATDDD                                                       
*                                                                               
         ORG   TWAHOLE                                                          
AMTBLK   DS    CL64                                                             
GSTSW    DS    CL1                                                              
CTAXBEF  DS    CL1                 USED CTAX BEFORE                             
KEEPPROV DS    CL2                 SAVE PROVINCE                                
TMPNET   DS    PL6                                                              
TMPGRS   DS    PL6                                                              
TMPGST   DS    PL6                                                              
TMPPST   DS    PL6                                                              
XTRAELM  DS    CL71                                                             
CLEAROK  DS    CL1                 FLAG FOR CLEAR GST/PST FIELD                 
USERPROV DS    CL1                 FLAG, USER ENTERED PROVINCE                  
USERPST  DS    CL1                 FLAG, USER ENTERED PST TYPE                  
*                                                                               
         EJECT                                                                  
         ORG   CONTABH                                                          
       ++INCLUDE ACBATC4D                                                       
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
PROGD    DSECT                                                                  
RELO1    DS    A                                                                
SAVERE   DS    A                                                                
WRKC     DS    CL2                                                              
JOBCASH  DS    PL6                                                              
SUPCASH  DS    PL6                                                              
INCCASH  DS    PL6                                                              
PL16     DS    PL16                                                             
CLINUM   DS    CL15                                                             
CLINAM   DS    CL36                                                             
PRODNUM  DS    CL15                                                             
PRODNAM  DS    CL36                                                             
JOBNUM   DS    CL15                                                             
JOBNAM   DS    CL36                                                             
COSTBYTE DS    CL1                                                              
COSTING  DS    CL15                                                             
COSTNAM  DS    CL36                                                             
SUPNUM   DS    CL15                                                             
SUPNAM   DS    CL36                                                             
SUPTYPE  DS    CL1                                                              
SUPPSTT  DS    CL1                                                              
VENDPROV DS    CL2                                                              
CINCNUM  DS    CL15                                                             
CINCNAM  DS    CL36                                                             
DOCSAVE  DS    CL6                                                              
DATE     DS    CL3                                                              
DISC     DS    D                     FIRST CD RATE, THEN CD AMNT                
INCNUM   DS    CL15                                                             
INCNAM   DS    CL36                                                             
FILT     DS    CL2                                                              
         SPACE 1                                                                
GST      DS    PL6                                                              
PST      DS    PL6                                                              
SUPNET   DS    PL6                                                              
TOTNET   DS    PL6                 AFTER CALCULATING GST/PST                    
TOTGRS   DS    PL6                                                              
GORN     DS    CL1                                                              
GSTINP   DS    CL1                                                              
         SPACE 1                                                                
AINCH    DS    A                                                                
AINCNH   DS    A                                                                
AINCAH   DS    A                                                                
ANARH    DS    A                                                                
ATYPEH   DS    A                                                                
ATYPNH   DS    A                                                                
AGORNH   DS    A                                                                
AGSTXH   DS    A                                                                
AGAMTH   DS    A                                                                
APROVH   DS    A                                                                
         SPACE 1                                                                
SIMDSW   DS    CL1                   PASS CD ON TO CLIENT ?                     
SIMDAC   DS    CL15                                                             
SIMDACN  DS    CL36                                                             
*                                                                               
NEWCOST  DS    CL12                NEW COSTING CODE                             
DISCACC  DS    CL15                ACCOUNT FOR C.D. NOT PASSED ON               
ELCODE   DS    CL1                                                              
       ++INCLUDE ACBATCTAX                                                      
*                                                                               
TMPMODE  DS    CL1                                                              
KEY      DS    CL49                                                             
IOAREA   DS    CL2000                                                           
PROGDX   DS    0C                                                               
         EJECT                                                                  
*FAUTL                                                                          
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENDAY                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
*DDFLDIND                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACBAT21   01/27/04'                                      
         END                                                                    
