*          DATA SET ACBAT08    AT LEVEL 028 AS OF 08/10/10                      
*PHASE T61B08A                                                                  
         TITLE 'INTERNAL INVOICES'                                              
T61B08   CSECT                                                                  
*----------------------------------------------------------------               
*        INTERNAL INVOICES                                                      
*----------------------------------------------------------------               
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,**BAT08,R7,CLEAR=YES,RR=2                           
         USING TWAD,RA                                                          
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING PROGD,RC                                                         
         LA    R2,INTDOCH                                                       
         BAS   RE,ANY                                                           
         MVC   SAVEDOC,INTDOC                                                   
         OC    SAVEDOC,SPACES                                                   
         LA    R2,INTDATH                                                       
         MVI   ERRNUM,13                                                        
         CLI   INTDATH+5,0                                                      
         BNE   *+12                                                             
         BAS   RE,GETODAY                                                       
         B     IT1                                                              
*                                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         CLC   WORK(2),=C'70'      NOT BEFORE 1970                              
         BL    ERROR                                                            
*                                                                               
IT1      GOTO1 DATCON,DMCB,(0,WORK),(1,SAVEDATE)                                
         GOTO1 DATECHK,DMCB,SAVEDATE                                            
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
IT1A     DS    0H                                                               
*&&US                                                                           
*              CLEAR SALES TAX NAME FIELDS                                      
         MVC   INTSTN1,SPACES                                                   
         OI    INTSTN1H+6,X'80'                                                 
         MVC   INTSTN2,SPACES                                                   
         OI    INTSTN2H+6,X'80'                                                 
*&&                                                                             
         EJECT                                                                  
*----------------------------------------------------------------               
*        VALIDATE INPUT ACCOUNTS & SAVE NAMES/NUMBERS                           
*----------------------------------------------------------------               
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         LA    R2,INTCLIH                                                       
         BAS   RE,ANY                                                           
*                                                                               
         MVI   ERRNUM,37                                                        
         CLC   5(1,R2),CLILNGTH                                                 
         BH    ERROR                                                            
*                                                                               
         SR    R3,R3                                                            
         IC    R3,INTCLIH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),INTCLI                                                  
         LA    R8,COMPEL                                                        
         USING ACCOMPD,R8                                                       
         MVC   KEY+1(2),ACMPJOB    PRODUCTION LEDGER                            
*                                                                               
         MVC   SVCLIH,INTCLIH      SAVE CLIENT HEADER INFO                      
*                                                                               
         BAS   RE,CLEARX           ALWAYS CLEAR AND START OVER                  
*                                                                               
         NI    INTPROH+4,X'DF'                                                  
         NI    INTJOBH+4,X'DF'                                                  
*                                                                               
         MVC   INTCLIN,SPACES      CLEAR CLIENT NAME                            
         OI    INTCLINH+6,X'80'                                                 
*                                                                               
         MVC   INTPRON,SPACES      CLEAR PRODUCT NAME                           
         OI    INTPRONH+6,X'80'                                                 
*                                                                               
         MVC   INTJOBN,SPACES      CLEAR JOB NAME                               
         OI    INTJOBNH+6,X'80'                                                 
*                                                                               
         XC    CLIPROF,CLIPROF                                                  
         XC    PRODPROF,PRODPROF                                                
         XC    JOBPROF,JOBPROF                                                  
         LA    R6,CLIPROF                                                       
         BAS   RE,GETACC                                                        
         MVI   ERRNUM,28                                                        
         TM    ACCTSTAT,X'10'      IS CLIENT LOCKED ?                           
         BO    BADACC              YES, ERROR                                   
*                                                                               
         MVC   INTCLIN,ACCTNAME                                                 
         OI    INTCLINH+6,X'80'                                                 
         OI    INTCLIH+4,X'20'                                                  
*                                                                               
IN2      LA    R2,INTPROH                                                       
         BAS   RE,ANY                                                           
*                                                                               
         MVC   SVPROH,INTPROH      SAVE PRODUCT HEADER INFO                     
*                                                                               
         LA    R1,KEY+3                                                         
         IC    R3,CLILNGTH         LEVA LENGTH                                  
         AR    R1,R3               READY FOR PRODUCT                            
         IC    R3,INTPROH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),INTPRO                                                   
*                                                                               
         LA    R6,PRODPROF                                                      
         BAS   RE,GETACC                                                        
         MVI   ERRNUM,28                                                        
         TM    ACCTSTAT,X'10'      IS PRODUCT LOCKED ?                          
         BO    BADACC              YES, ERROR                                   
*                                                                               
         MVC   INTPRON,ACCTNAME                                                 
         OI    INTPRONH+6,X'80'                                                 
         OI    INTPROH+4,X'20'                                                  
*                                                                               
         MVC   PRODNUM,KEY                                                      
         MVC   PRODNAM,INTPRON                                                  
         OC    PRODNAM,SPACES                                                   
*&&US                                                                           
         SR    R6,R6               LOOK FOR SALES ELEMENT                       
         BAS   RE,HIGH                                                          
         LA    R1,IOAREA                                                        
*                                                                               
IN4A     CLI   0(R1),0                                                          
         BE    IN4E                                                             
         CLI   0(R1),X'3D'                                                      
         BE    IN4C                                                             
         ZIC   RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     IN4A                                                             
*                                                                               
         USING ACSAND,R1                                                        
IN4C     MVC   SAVEKEY,KEY                                                      
         MVC   KEY(15),ACSACODE                                                 
         MVC   PRODNUM,ACSACODE                                                 
         BAS   RE,GETACC                                                        
         MVC   PRODNAM,ACCTNAME                                                 
         MVC   KEY(15),SAVEKEY                                                  
*                                                                               
IN4E     DS    0H                                                               
*&&                                                                             
         LA    R2,INTJOBH                                                       
         BAS   RE,ANY                                                           
*                                                                               
         MVC   SVJOBH,INTJOBH      SAVE JOB HEADER INFO                         
*                                                                               
         LA    R1,KEY+3                                                         
         IC    R3,PRDLNGTH         LEVB LENGTH                                  
         AR    R1,R3                                                            
         IC    R3,INTJOBH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),INTJOB                                                   
*                                                                               
         XC    JOBPROF,JOBPROF                                                  
         LA    R6,JOBPROF                                                       
         BAS   RE,GETACC                                                        
         MVI   ERRNUM,18                                                        
         TM    ACCTSTAT,X'80'      DOES JOB HAVE BALANCE ELEMENT ?              
         BZ    BADACC              NO, ERROR                                    
         MVI   ERRNUM,23                                                        
         TM    ACCTSTAT,X'20'      IS JOB CLOSED ?                              
         BO    BADACC              YES, ERROR                                   
         MVI   ERRNUM,28                                                        
         TM    ACCTSTAT,X'10'      IS JOB LOCKED ?                              
         BO    BADACC              YES, ERROR                                   
*                                                                               
         MVC   INTJOBN,ACCTNAME                                                 
         OI    INTJOBNH+6,X'80'                                                 
         OI    INTJOBH+4,X'20'                                                  
*                                                                               
         DS    0H                  VALIDATE WORK CODE                           
         LA    R2,INTWRKH                                                       
         BAS   RE,ANY                                                           
         MVI   ERRNUM,19                                                        
         CLC   INTWRK(2),=C'99'                                                 
         BE    ERROR                                                            
         MVC   JOBNUM,KEY                                                       
*                                                                               
*&&US                                                                           
         LA    R4,JOBNUM                                                        
         GOTO1 ASETJOB,DMCB,(R4)                                                
         MVC   JOBSTAT,ACOPSTAT    SAVE JOB STATUS                              
*&&                                                                             
*                                                                               
         BAS   RE,PROFMERG                                                      
         LA    R4,PROFILE                                                       
         USING ACPROFD,R4                                                       
         MVC   CPJNUM,ACPRCOST     CLIENT COSTING FROM PROFILE                  
         MVC   OFFICE,ACPROFFC                                                  
*                                                                               
         USING GOXBLOCK,R4                                                      
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    IN6A0               NO, LEAVE CLIENT/PRODUCT OFFICE              
         L     R4,AGOXBLK                                                       
         OC    GOAWOFOF,GOAWOFOF   DOES OPTION MAINT HAVE AN OFFICE ?           
         BZ    *+10                NO, LEAVE IT ALONE                           
         MVC   OFFICE,GOAWOFOF     YES, SAVE THAT ONE                           
         DROP  R4                                                               
*                                                                               
IN6A0    LA    R2,INTFOFH                                                       
         CLI   5(R2),0             YES, WAS ANYTHING ENTERED ?                  
         BE    IN6A3               NO, LEAVE IT ALONE                           
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BO    IN6A1               YES, CONTINUE.....                           
         MVI   ERRNUM,INVALID      PREPARE FOR ERROR                            
         CLC   INTCR(3),=C'*SK'    IS CREDIT AN SK ACCOUNT ?                    
         BE    ERROR               YES, CAN'T OVERRIDE FINANCIAL THEN           
         B     IN6A2                                                            
*                                                                               
IN6A1    TM    4(R2),X'80'         WAS DATA ENTERED THIS TIME ?                 
         BO    IN6A2               YES, USE IT                                  
         MVC   FLD,SPACES          NO, CLEAR IT                                 
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
         B     IN6A3                                                            
*                                                                               
IN6A2    MVC   OFFICE,SPACES       YES, THAT'S THE ONE WE WANT                  
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OFFICE(0),8(R2)                                                  
         GOTO1 AVALOFFC,DMCB,(X'80',OFFICE)                                     
         BNE   ERROR                                                            
*                                                                               
IN6A3    OI    4(R2),X'20'         FINANCIAL OFFICE VALIDATED                   
         LA    R2,INTWRKH                                                       
         GOTO1 AGETWC,INTWRK                                                    
         BNE   ERROR                                                            
         MVC   INTWRKN,WORK                                                     
         OI    INTWRKNH+6,X'80'                                                 
*                                                                               
*&&US                                                                           
         USING GOBLOCKD,R1                                                      
         L     R1,AGOBLOCK                                                      
         LA    R1,GOUWLIST         CHECK IT'S A BILLABLE W/C                    
         BAS   RE,IN6AA                                                         
         B     IN6AC                                                            
         DROP  R1                                                               
*                                                                               
IN6AA    LA    R0,6                                                             
IN6AB    CLC   INTWRK,0(R1)                                                     
         BE    ERROR                                                            
         LA    R1,2(R1)                                                         
         BCT   R0,IN6AB                                                         
         BR    RE                                                               
*&&                                                                             
IN6AC    LA    R2,INTAMTH                                                       
         BAS   RE,ANY                                                           
         IC    R3,INTAMTH+5                                                     
         MVI   ERRNUM,25                                                        
         GOTO1 AMTVAL,DMCB,INTAMT,(R3)                                          
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         L     RF,DMCB+4                                                        
         LA    RF,0(RF)                                                         
         ZAP   INCAMT,0(8,RF)                                                   
         MVC   FVMSGNO,=AL2(AE$AMLGE)                                           
         CP    INCAMT,=P'2100000000'                                            
         BH    ERROR                                                            
         CP    INCAMT,=P'-2100000000'                                           
         BL    ERROR                                                            
*                                                                               
*&&US                                                                           
         MVI   ERRNUM,25                                                        
         LA    R3,JOBNUM           RE-READ THE JOB                              
         LA    RE,INTWRK                                                        
         GOTO1 ASETJOB,DMCB,(X'80',(R3)),(RE)                                   
*                                                                               
         GOTO1 AOPTVAL                                                          
         BNE   ERROR                                                            
*                                                                               
         CLI   CHECK,C' '          CHECKING AMOUNT ?                            
         BE    IN6AE               NO                                           
         MVI   ERRNUM,30                                                        
         MVC   WRKC,INTWRK                                                      
         ZAP   WRKCAMT,INCAMT                                                   
         ZAP   DUB1,INCAMT         NEED THIS FOR SALES TAX                      
         LA    R3,WRKC                                                          
         GOTO1 AWRKVAL,DMCB,(R3)                                                
         BH    ERROR                                                            
*&&                                                                             
IN6AE    DS    0H                                                               
         MVC   KEY(1),COMPANY                                                   
         MVC   JOBNAME,INTJOBN     NAME FROM SCREEN                             
         OC    JOBNAME,SPACES                                                   
         SR    R6,R6                                                            
         EJECT                                                                  
*                                                                               
IN6A5    BAS   RE,CHK1C            MAKE SURE 1C HASN'T CHANGED YET              
         LA    R2,INTCRH           CREDIT ACCOUNT                               
         BAS   RE,ANY              MAKE SURE INPUT IN FIELD                     
*                                                                               
IN6A6    DS    0H                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY      COMPANY CODE                                 
         MVC   KEY+1(2),=C'SI'     CR SI ACCOUNT                                
         MVC   KEY+3(12),INTCR                                                  
         OC    KEY+1(14),SPACES    PAD WITH BLANKS                              
*                                                                               
         CLI   INTCR,C'*'          SK OVERRIDE IN EFFECT?                       
         BNE   IN6A7               NO, SO JUST USE SI ACCOUNT                   
         MVI   ERRNUM,18                                                        
         CLC   INTCR+1(2),=C'SK'                                                
         BNE   ERROR               MAKE SURE IT WAS AN SK ACCT OVERRIDE         
*                                                                               
         MVC   KEY+1,SPACES                                                     
         ZIC   RF,5(R2)            GET LENGTH OF INPUT                          
         SH    RF,=H'2'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+1(0),INTCR+1    LOAD IN SK ACCNT                             
*                                                                               
IN6A7    SR    R6,R6                                                            
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   CRNUM,ACCTNUM       GET CREDIT ACCNT                             
         MVC   CRNAME,ACCTNAME                                                  
*                                                                               
         MVC   COSTAC,SPACES                                                    
         MVC   KEY,SPACES          READ FOR CREDIT ACCOUNT                      
         MVC   KEY(15),CRNUM                                                    
*                                                                               
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    IN6A8               NO                                           
         CLC   CRNUM+1(2),=C'SK'   YES, IS THIS AN SK ACCOUNT ?                 
         BNE   IN6A8               NO, SKIP OVER                                
         MVI   KEY+2,C'I'          YES, READ FOR SI ACCOUNT ALSO                
         BAS   RE,GETACC                                                        
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         TM    ACCTSTAT,ACSBAL     DOES RECORD HAVE BALANCE ELEMENT ?           
         BZ    BADACC              NO                                           
*                                                                               
IN6A8    TM    COMPSTAT,X'10'      COSTING REQUIRED                             
         BZ    IN8                 NO                                           
         BAS   RE,READ                                                          
         MVI   ELCODE,X'2C'        LOOK FOR COST ACCOUNT OVERRIDE               
         L     R4,AIOAREA1                                                      
         BAS   RE,GETEL                                                         
         BNE   IN6B1                                                            
*                                                                               
         USING ACSPECD,R4                                                       
IN6A9    DS    0H                                                               
         CLI   ACSPTYP,ACSPOAN     X'03' IS TYPE INDICATOR FOR ANALYSIS         
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BNE   IN6B1                                                            
         B     IN6A9               LOOP BACK UP                                 
         MVC   COSTAC,ACSPACCT                                                  
         B     IN8                                                              
*                                                                               
         USING ACSTATD,R4                                                       
IN6B1    MVI   ELCODE,X'30'                                                     
         L     R4,AIOAREA1                                                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                30 ELEMENT MUST EXIST                        
*                                                                               
         MVC   COSTAC(1),ACSTCOST                                               
         OC    ACSTCNTR,SPACES                                                  
         CLC   ACSTCNTR,SPACES     IF ALL SPACES THEN NO OVERRIDE               
         BE    IN8                                                              
*                                                                               
         CLI   ACSTCPOS,0          IF 0 START AT AT KEY+7                       
         BE    *+20                                                             
         LA    R5,CPJNUM+2         ADDR OF KEY+2                                
         ZIC   R0,ACSTCPOS         STARTING POINT NUMBER                        
         AR    R5,R0               NEW STARTING POINT                           
         B     *+8                                                              
         LA    R5,CPJNUM+7         CLIENT COSTING KEY+7                         
         LA    R3,3                                                             
         LA    R4,ACSTCNTR                                                      
*                                                                               
         CLI   0(R4),C' '          REPLACE ONLY NON-BLANK                       
         BE    *+10                                                             
         MVC   0(1,R5),0(R4)                                                    
         LA    R5,1(R5)                                                         
         LA    R4,1(R4)                                                         
         BCT   R3,*-22                                                          
*                                                                               
IN8      DS    0H                                                               
         MVC   INTCR,SPACES                                                     
         CLC   CRNUM+1(2),=C'SK'                                                
         BNE   IN8A                                                             
         MVI   INTCR,C'*'                                                       
         MVC   INTCR+1(L'INTCR-1),CRNUM+1                                       
         B     *+10                                                             
*                                                                               
IN8A     MVC   INTCR(12),CRNUM+3       DISPLAY ACCOUNT                          
         OI    INTCRH+6,X'80'                                                   
         MVC   INTCRNM,CRNAME                                                   
         OI    INTCRNMH+6,X'80'                                                 
*                                                                               
         TM    COMPSTAT,X'10'      COSTING REQUIRED                             
         BZ    IN8F                                                             
*                                                                               
         MVC   KEY(15),CPJNUM                                                   
         LA    R2,INTCLIH                                                       
*&&US                                                                           
         USING ACPROFD,RF                                                       
         LA    RF,PROFILE                                                       
         CLC   CPJNUM,ACPRCOST                                                  
         BE    *+8                                                              
         LA    R2,INTCRH                                                        
         DROP  RF                                                               
*&&                                                                             
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   CPJNAME,ACCTNAME                                                 
*                                                                               
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BO    *+14                YES, READ COSTING ACCOUNT                    
         CLC   CRNUM+1(2),=C'SK'   NO COSTING FOR SUSPENSE INCOME               
         BE    IN8F                                                             
         MVC   KEY+3(12),SPACES                                                 
         MVC   KEY+1(2),=C'12'     HARD U/L FOR COSTING COMMISSIONS             
         MVC   KEY+3(12),COSTAC    COSTING ACCOUNT FROM NOMINAL LEDGER          
         BAS   RE,GETACC                                                        
         LA    R2,INTCRH                                                        
         BAS   RE,CHECKACC                                                      
         MVC   CNUM,ACCTNUM                                                     
         MVC   CNAME,ACCTNAME                                                   
         DROP  R8                                                               
*                                                                               
IN8F     DS    0H                                                               
         LA    R2,INTCOMH          ANY INPUT IN 'COMMISSIONABLE' FIELD          
         CLI   5(R2),0                                                          
         BE    IN9                                                              
         CLI   INTCOM,C'Y'                                                      
         BE    IN9                                                              
         MVI   ERRNUM,2                                                         
         CLI   INTCOM,C'N'                                                      
         BNE   ERROR                                                            
         EJECT                                                                  
*----------------------------------------------------------------               
*        ALLOW SALES TAX (US ONLY)                                              
*----------------------------------------------------------------               
*                                                                               
*&&UK                                                                           
IN9      B     IN10                                                             
*&&                                                                             
*&&US                                                                           
IN9      LA    R2,INTEXPH                                                       
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BO    IN9B                YES                                          
         MVI   ERRNUM,44           NO, ANALYSIS AND EXPENSE ARE INVALID         
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
         LA    R2,INTAOFH                                                       
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
         LA    R2,INTDEPH                                                       
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
         LA    R2,INTSTFH                                                       
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
         B     IN9Z                                                             
*                                                                               
         USING GOXBLOCK,R4                                                      
IN9B     L     R4,AGOXBLK          GET EXTENDED GOBLOCK                         
         LA    R2,INTEXPH                                                       
         CLI   5(R2),0             WAS EXPENSE ENTERED ?                        
         BNE   IN9D                YES, USE IT                                  
*                                                                               
         MVC   FLD,SPACES                                                       
         OC    GOAWOA,GOAWOA       NO, IS THERE ONE IN OPTION MAINT ?           
         BZ    IN9C                NO, FORCE ERROR                              
         CLC   GOAWOA(2),=C'SE'    IS THIS AN 'SE' ACCOUNT ?                    
         BE    *+18                YES                                          
         MVI   FLD,C'*'            NO, INDICATE OVERRIDE                        
         MVC   FLD+1(L'GOAWOA),GOAWOA                                           
         B     *+10                                                             
         MVC   FLD(L'GOAWOA-2),GOAWOA+2                                         
*                                                                               
IN9C     BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
         GOTO1 ANY                 X-JOBS MUST HAVE EXPENSE                     
*                                                                               
IN9D     OI    4(R2),X'20'         EXPENSE VALIDATED                            
         LA    R2,INTFOFH                                                       
         CLI   5(R2),0             WAS A FINANCIAL OFFICE ENTERED ?             
         BNE   IN9E                YES, USE IT                                  
*                                                                               
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'GOAWOFOF),GOAWOFOF                                         
         OC    GOAWOFOF,GOAWOFOF                                                
         BNZ   *+10                                                             
         MVC   FLD(L'OFFICE),OFFICE                                             
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
IN9E     MVC   KEY,SPACES          READ OFFICE FOR NAME                         
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'2D'                                                  
         MVC   KEY+3(2),OFFICE                                                  
         BAS   RE,GETACC                                                        
*                                                                               
         LA    R2,INTFOFNH                                                      
         MVC   FLD,SPACES                                                       
         MVC   FLD,ACCTNAME                                                     
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
         LA    R2,INTFOFH                                                       
         OI    4(R2),X'20'         FINANCIAL OFFICE VALIDATED                   
         LA    R2,INTAOFH                                                       
         CLI   5(R2),0             WAS AN ANALYSIS OFFICE ENTERED ?             
         BNE   IN9F                YES, USE IT                                  
*                                                                               
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'GOAWOAOF),GOAWOAOF                                         
         OC    GOAWOAOF,GOAWOAOF                                                
         BNZ   *+10                                                             
         MVC   FLD(L'OFFICE),OFFICE                                             
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
IN9F     OI    4(R2),X'20'         ANALYSIS OFFICE VALIDATED                    
         LA    R2,INTDEPH                                                       
         CLI   5(R2),0             WAS DEPARTMENT ENTERED ?                     
         BNE   IN9G                YES, USE IT                                  
*                                                                               
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'GOAWODEP),GOAWODEP                                         
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
IN9G     OI    4(R2),X'20'         DEPARTMENT VALIDATED                         
         LA    R2,INTSTFH                                                       
         CLI   5(R2),0             WAS STAFF ENTERED ?                          
         BNE   IN9H                YES, USE IT                                  
*                                                                               
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'GOAWOSTF),GOAWOSTF                                         
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
         DROP  R4                                                               
*                                                                               
         USING EXCELD,R4                                                        
IN9H     OI    4(R2),X'20'         FINANCIAL OFFICE VALIDATED                   
         LA    R4,EXCWORK          ADDRESS EXCEL WORKAREA                       
         MVI   EXCACT,EXCAVAL      VALIDATE MODE                                
         LA    RF,INTAOFH          A(ANALYSIS OFFICE HEADER)                    
         ST    RF,EXCAANO                                                       
         LA    RF,INTDEPH          A(DEPARTMENT HEADER)                         
         ST    RF,EXCADEP                                                       
         LA    RF,INTSTFH          A(STAFF HEADER)                              
         ST    RF,EXCASTF                                                       
         LA    RF,INTEXPH          A(EXPENSE ACCOUNT HEADER)                    
         ST    RF,EXCAEXP                                                       
         ST    R9,EXCAGWS          A(GLOBAL WORKING STORAGE)                    
         MVC   EXCSECAC,CRNUM      INCOME ACCOUNT NUMBER                        
         MVC   EXCSECAN,CRNAME     INCOME ACCOUNT NAME                          
         GOTO1 VEXCEL,EXCELD                                                    
         BNE   CURSIT              EXIT IMMEDIATELY                             
*                                                                               
         MVC   FLD,SPACES          MOVE NAMES TO SCREEN                         
         MVC   FLD(L'EXCSENM),EXCSENM                                           
         LA    R2,INTEXPNH                                                      
         BAS   RE,MOVEFLD                                                       
*                                                                               
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'EXC2PNM),EXC2PNM                                           
         CLC   FLD,SPACES                                                       
         BH    *+10                                                             
         MVC   FLD(L'EXC2DNM),EXC2DNM                                           
         LA    R2,INTSTFNH                                                      
         BAS   RE,MOVEFLD                                                       
         DROP  R4                                                               
         EJECT                                                                  
IN9Z     LA    R2,INTSTX1H                                                      
         LA    R4,TAX1                                                          
         BAS   RE,VALWAM           VALIDATE SALES TAX FIELD 1                   
         BNE   ERROR                                                            
         LA    R2,INTSTX2H                                                      
         LA    R4,TAX2                                                          
         BAS   RE,VALWAM           VALIDATE SALES TAX FIELD 2                   
         BNE   ERROR                                                            
         B     IN10                                                             
         EJECT                                                                  
*----------------------------------------------------------------               
*        ROUTINE TO VALIDATE UP TO 2 WORK CODES AND 2 AMOUNTS                   
*----------------------------------------------------------------               
*                                                                               
*        R2=A(TAX FIELD HDR),R4=A(OUTPUT VALUES)                                
*                                                                               
         USING STXD,R4                                                          
VALWAM   NTR1                                                                   
         MVI   FVERRNDX,0                                                       
         XC    STXACC,STXACC                                                    
         XC    STXWK,STXWK                                                      
         ZAP   STXAMT,=P'0'                                                     
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VALW02              YES                                          
         CLI   CHECK,C'T'          NO, ARE WE CHECKING TOTAL                    
         BNE   VALWOK              NO, OK TO EXIT                               
         LA    RF,TAX1                                                          
         CR    R4,RF               ARE WE AT THE FIRST TAX FIELD?               
         BE    VALWOK              YES, OK TO EXIT                              
         B     VALW14              NO, HANDLE DUB1                              
*                                                                               
VALW02   MVI   ERRNUM,47                                                        
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BO    ERROR                                                            
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(3,BLOCK)                                      
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         CLI   4(R1),1                                                          
         BL    ERROR               NOTHING ENTERED                              
         MVC   FVMSGNO,=AL2(AE$WCREQ)                                           
         BE    ERROR                                                            
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         CLI   4(R1),3                                                          
         BH    ERROR                                                            
         MVC   FVMSGNO,=AL2(AE$AMREQ)                                           
         BL    ERROR                                                            
         MVC   FVMSGNO,=AL2(AE$INACC)                                           
*                                                                               
         LA    R5,BLOCK                                                         
         CLI   1(R5),0                                                          
         BNE   ERROR                                                            
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         CLI   12(R5),C'*'         IS THERE A LEDGER OVERRIDE                   
         BNE   VALW04                                                           
         CLI   0(R5),15            MUST INCLUDE * FOR LENGTH                    
         BH    ERROR                                                            
         MVC   KEY+1(14),13(R5)                                                 
         CLC   13(2,R5),=C'SV'                                                  
         BE    VALW06              SV IS VALID OVERIDE                          
         CLC   13(2,R5),=C'SB'                                                  
         BE    VALW06              SB IS VALID OVERIDE                          
         B     ERROR                                                            
*                                                                               
VALW04   MVC   KEY+1(2),=C'SC'     DEFAULT IS SC                                
         MVI   ERRNUM,2                                                         
         CLI   0(R5),12                                                         
         BH    ERROR                                                            
         MVC   KEY+3(12),12(R5)                                                 
*                                                                               
VALW06   BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   STXACC,ACCTNUM                                                   
         MVC   STXNAM,ACCTNAME                                                  
         ZIC   R1,0(R2)                                                         
         LA    R3,0(R1,R2)         R3 TO NAME FIELDS                            
         MVC   8(36,R3),STXNAM        NAME TO SCREEN                            
         OI    6(R3),X'80'                                                      
*                                                                               
         ZIC   R1,0(R5)            LENGTH OF FIELD                              
         LA    R1,1(R1)            INCLUDE LENGTH OF COMMA                      
         STC   R1,FVERRNDX                                                      
         LA    R5,32(R5)           R5=A(SCAN BLOCK ENTRY FOR WC)                
         MVC   FVMSGNO,=AL2(AE$INWRK)                                           
         CLI   0(R5),1             CHECK L'INPUT                                
         BL    ERROR                                                            
         CLI   0(R5),2                                                          
         BH    ERROR                                                            
         CLI   1(R5),0                                                          
         BNE   ERROR                                                            
         MVC   STXWK,12(R5)                                                     
         CLC   STXWK,=C'99'                                                     
         BE    ERROR                                                            
*&&US                                                                           
         USING GOBLOCKD,R1                                                      
         L     R1,AGOBLOCK                                                      
         LA    R1,GOUWLIST         CHECK IT'S A BILLABLE W/C                    
         BAS   RE,VALW08                                                        
         B     VALW12                                                           
         DROP  R1                                                               
*                                                                               
VALW08   LA    R0,6                                                             
*                                                                               
VALW10   CLC   STXWK,0(R1)                                                      
         BE    ERROR                                                            
         LA    R1,2(R1)                                                         
         BCT   R0,VALW10                                                        
         BR    RE                                                               
*&&                                                                             
*                                  CHECK IT'S A BILLABLE W/C                    
*&&UK                                                                           
         LA    R1,CLIPROF                                                       
         BAS   RE,VALW08                                                        
         LA    R1,PRODPROF                                                      
         BAS   RE,VALW08                                                        
         LA    R1,JOBPROF                                                       
         BAS   RE,VALW08                                                        
         B     VALW12                                                           
*                                                                               
         USING ACPROFD,R1                                                       
VALW08   LA    R0,6                                                             
*                                                                               
VALW10   CLC   STXWK,ACPRUNBL                                                   
         BE    ERROR                                                            
         LA    R1,2(R1)                                                         
         BCT   R0,VALW10                                                        
         BR    RE                                                               
*&&                                                                             
*                                  NOW READ ANALYSIS RECORD                     
VALW12   GOTO1 AGETWC,STXWK                                                     
         BNE   ERROR                                                            
         MVC   WORK+15(L'WORK-L'WORK+15),SPACES                                 
         MVC   WORK+38(L'ACANDESC),WORK                                         
         MVI   WORK+37,C'/'                                                     
         MVC   WORK(36),STXNAM                                                  
         GOTO1 SQUASHER,DMCB,WORK,50                                            
         ZIC   RF,0(R2)                                                         
         LA    R3,8(RF,R2)         R3 TO NAME FIELDS                            
         MVC   0(L'INTSTN1,R3),WORK                                             
*                                  VALIDATE AMOUNTS                             
         ZIC   R1,0(R5)            LENGTH OF FIELD                              
         LA    R1,1(R1)            INCLUDE LENGTH OF COMMA                      
         SR    RE,RE                                                            
         IC    RE,FVERRNDX         ADD TO PREVIOUS INDEX                        
         AR    R1,RE                                                            
         STC   R1,FVERRNDX                                                      
         LA    R5,32(R5)                                                        
         MVI   ERRNUM,2                                                         
         CLI   1(R5),0                                                          
         BNE   ERROR                                                            
         ZIC   R0,0(R5)            CHECK FOR VALID CASH FILED                   
         GOTO1 AMTVAL,DMCB,12(R5),(R0)                                          
         MVI   ERRNUM,25                                                        
         CLI   0(R1),0                                                          
         BNE   ERROR                                                            
         L     R1,4(R1)                                                         
         LA    R1,0(R1)                                                         
         ZAP   STXAMT,0(8,R1)                                                   
*                                                                               
*&&US                                                                           
         LA    R3,JOBNUM           RE-READ THE JOB                              
         LA    RE,STXWK                                                         
         GOTO1 ASETJOB,DMCB,(X'80',(R3)),(RE)                                   
*                                                                               
         GOTO1 AOPTVAL                                                          
         BNE   ERROR                                                            
*                                                                               
         CLI   CHECK,C' '          CHECKING AMOUNT ?                            
         BE    VALWOK              NO                                           
         CLI   CHECK,C'T'          YES, CHECKING TOTAL?                         
         BNE   VALW16              NO                                           
         AP    DUB1,STXAMT         UPDATE TOTAL WITH THIS ITEM                  
         LA    RF,TAX2                                                          
         CR    R4,RF               YES, ARE WE AT 2ND AMOUNT?                   
         BNE   VALWOK              NO, SKIP IT FOR NOW                          
*                                                                               
VALW14   LA    R3,DUB1             USE TOTAL                                    
         B     *+8                                                              
*                                                                               
VALW16   LA    R3,STXWK                                                         
         GOTO1 AWRKVAL,DMCB,(R3)                                                
         BH    ERROR                                                            
*&&                                                                             
*                                                                               
VALWOK   MVI   ERRNUM,X'FF'        SET CC=EQ IF ALL OK                          
         MVI   FVERRNDX,0          RESET ERROR INDEX                            
VALWERR  CLI   ERRNUM,X'FF'                                                     
         XIT1  REGS=(R2)                                                        
*&&                                                                             
         EJECT                                                                  
*----------------------------------------------------------------               
*        BUILD ELEMENTS FOR ACCDAY                                              
*----------------------------------------------------------------               
*                                                                               
IN10     LA    R8,IOAREA+2                                                      
         USING DLDESCD,R8                                                       
         MVI   DLDSEL,X'64'        DESCRIPTION ELEMENT                          
         MVC   DLDSREF,SAVEDOC                                                  
         MVC   DLDSDATE,SAVEDATE                                                
*                                                                               
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
*&&UK*&& OI    DLDSSTAT,X'08'      AUTHORISE                                    
         LA    R2,INTNARH                                                       
         LA    R3,DLDSNARR                                                      
         XC    DLDSNARR,DLDSNARR                                                
         BAS   RE,NARRSCAN                                                      
         SR    R3,R3                                                            
         LA    R5,DLDSNARR                                                      
         SR    R5,R8               L'ELEMENT-NARRATIVE                          
         AR    R5,R6               R6=L'NARRATIVE                               
         STC   R5,DLDSLEN          L'ELEMENT                                    
         AR    R8,R5               BUMP TO FIRST POSTING ELEMENT                
*&&UK                                                                           
         USING TRBDETD,R8                                                       
         MVC   TRBDEL(2),=X'4B10'  BILL DATE/NUMBER ELEMENT                     
         MVC   TRBDNO,SPACES                                                    
         XC    TRBDDTE(8),TRBDDTE                                               
         LA    R8,16(R8)                                                        
*&&                                                                             
         USING DLPOSTD,R8                                                       
         ZAP   TRANSAMT,=P'0'                                                   
*&&US                                                                           
         USING STXD,R4                                                          
IN10A    LA    R4,TAX1                                                          
         OC    STXACC,STXACC       ANY SALES TAX?                               
         BZ    IN10B               NO                                           
*                                                                               
         CLC   STXACC+1(2),=C'SV'  YES, IS IT A PAYABLE ACCOUNT?                
         BNE   IN10AA              NO                                           
*                                                                               
         USING PAKELD,R8                                                        
         MVI   PAKEL,PAKELQ        YES, ADD AN ELEMENT FOR IT                   
         MVI   PAKLN,PAKLNQ                                                     
         MVC   PAKACC,STXACC                                                    
         MVC   PAKOFF,OFFICE                                                    
         MVC   PAKCON,JOBNUM                                                    
         MVC   PAKDATE,SAVEDATE                                                 
         MVC   PAKREF,SAVEDOC                                                   
         ZIC   R3,PAKLN                                                         
         AR    R8,R3                                                            
         DROP  R8                                                               
*                                                                               
         USING DLPOSTD,R8                                                       
IN10AA   MVI   DLPSEL,X'69'        SINGLE DEBIT                                 
         MVI   DLPSLEN,X'71'                                                    
         MVC   DLPSDBAC,JOBNUM     DEBIT JOB                                    
         MVC   DLPSDBNM,JOBNAME                                                 
         MVC   DLPSCRAC,STXACC      CREDIT VENDOR                               
         MVC   DLPSCRNM,STXNAM                                                  
         MVI   DLPSTYPE,X'40'      NON-COMMISSIONABLE                           
         MVC   DLPSANAL,STXWK                                                   
         ZAP   DLPSAMNT,STXAMT                                                  
         AP    TRANSAMT,STXAMT                                                  
         ZIC   R3,DLPSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
         MVI   DLPSEL,X'6A'        SINGLE CREDIT                                
         MVI   DLPSLEN,X'71'                                                    
         MVC   DLPSDBAC,JOBNUM                                                  
         MVC   DLPSDBNM,JOBNAME                                                 
         MVC   DLPSCRAC,STXACC      CREDIT VENDOR                               
         MVC   DLPSCRNM,STXNAM                                                  
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,STXAMT                                                  
         MVC   DLPSANAL,SPACES                                                  
         LA    RF,PROFILE                                                       
         MVC   DLPSANAL,OFFICE                                                  
         ZIC   R3,DLPSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
IN10B    LA    R4,TAX2                                                          
         OC    STXACC,STXACC       ANY SALES TAX?                               
         BZ    IN10C               NO                                           
         CLC   STXACC+1(2),=C'SV'  YES, IS IT A PAYABLE ACCOUNT?                
         BNE   IN10BB              NO                                           
*                                                                               
         USING PAKELD,R8                                                        
         MVI   PAKEL,PAKELQ        YES, ADD AN ELEMENT FOR IT                   
         MVI   PAKLN,PAKLNQ                                                     
         MVC   PAKACC,STXACC                                                    
         MVC   PAKOFF,OFFICE                                                    
         MVC   PAKCON,JOBNUM                                                    
         MVC   PAKDATE,SAVEDATE                                                 
         MVC   PAKREF,SAVEDOC                                                   
         ZIC   R3,PAKLN                                                         
         AR    R8,R3                                                            
         DROP  R8                                                               
*                                                                               
         USING DLPOSTD,R8                                                       
IN10BB   MVI   DLPSEL,X'69'        SINGLE DEBIT                                 
         MVI   DLPSLEN,X'71'                                                    
         MVC   DLPSDBAC,JOBNUM     DEBIT JOB                                    
         MVC   DLPSDBNM,JOBNAME                                                 
         MVC   DLPSCRAC,STXACC      CREDIT VENDOR                               
         MVC   DLPSCRNM,STXNAM                                                  
         MVI   DLPSTYPE,X'40'      NON-COMMISSIONABLE                           
         MVC   DLPSANAL,STXWK                                                   
         ZAP   DLPSAMNT,STXAMT                                                  
         AP    TRANSAMT,STXAMT                                                  
         ZIC   R3,DLPSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
         MVI   DLPSEL,X'6A'        SINGLE CREDIT                                
         MVI   DLPSLEN,X'71'                                                    
         MVC   DLPSDBAC,JOBNUM                                                  
         MVC   DLPSDBNM,JOBNAME                                                 
         MVC   DLPSCRAC,STXACC      CREDIT VENDOR                               
         MVC   DLPSCRNM,STXNAM                                                  
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,STXAMT                                                  
         MVC   DLPSANAL,SPACES                                                  
         LA    RF,PROFILE                                                       
         MVC   DLPSANAL,OFFICE                                                  
         ZIC   R3,DLPSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
IN10C    TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    IN10D               NO                                           
         USING TRCASHD,R8                                                       
         MVI   TRCSEL,TRCSELQ      YES, BUILD MEMO WITH AMOUNT                  
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'S'                                                    
         ZAP   TRCSAMNT,INCAMT                                                  
         SR    R3,R3                                                            
         IC    R3,TRCSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
         USING TRSDESCD,R8                                                      
         MVI   TRSDEL,TRSDELQ                                                   
         MVC   TRSDLEN,=YL1(L'ACKEYACC-1+2)                                     
         MVC   TRSDACCS(L'ACKEYACC-1),CRNUM+1                                   
         SR    R3,R3                                                            
         IC    R3,TRSDLEN                                                       
         AR    R8,R3                                                            
*                                                                               
         LA    R1,TRSELEM                                                       
         USING TRSELD,R1                                                        
         MVI   TRSEL,TRSELQ        BUILD SKELETAL TRANS STAT ELEM               
         MVI   TRSLN,TRSLNQ                                                     
         OI    TRSSTAT3,TRSSXJOB                                                
         MVC   0(L'TRSELEM,R8),TRSELEM  ADD TO POSTING                          
         LA    R8,L'TRSELEM(R8)                                                 
         DROP  R1                                                               
*                                                                               
*&&                                                                             
IN10D    LA    R1,ANOELEM                                                       
         USING ANOELD,R1                                                        
         MVI   ANOEL,ANOELQ        BUILD OFFICE ELEMENT                         
         MVI   ANOLN,ANOLNQ                                                     
         MVI   ANOTYPE,ANOTCLI                                                  
         MVC   ANOOFFC,OFFICE                                                   
         MVC   0(L'ANOELEM,R8),ANOELEM ADD TO POSTING                           
         LA    R8,L'ANOELEM(R8)                                                 
         DROP  R1                                                               
*                                                                               
         USING DLPOSTD,R8                                                       
IN10DD   MVI   DLPSEL,X'69'        SINGLE DEBIT                                 
         MVI   DLPSLEN,X'71'                                                    
         MVC   DLPSDBAC,JOBNUM                                                  
         MVC   DLPSDBNM,JOBNAME                                                 
         MVC   DLPSCRAC,CRNUM                                                   
         MVC   DLPSCRNM,CRNAME                                                  
         MVI   DLPSTYPE,0                                                       
         CLI   INTCOM,C'Y'                                                      
         BE    *+8                                                              
         MVI   DLPSTYPE,X'40'      NON-COMMISSIONABLE                           
         MVC   DLPSANAL,SPACES                                                  
         IC    R3,INTWRKH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   DLPSANAL(0),INTWRK                                               
         ZAP   DLPSAMNT,INCAMT                                                  
         AP    TRANSAMT,INCAMT                                                  
*                                                                               
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    IN10E               NO                                           
*                                                                               
         USING EXCELD,R4                                                        
         LA    R4,EXCWORK                                                       
         MVC   DLPSCRAC,EXCSEAC    YES, CHANGE CONTRA ACCOUNT                   
         MVC   DLPSCRNM,EXCSENM                                                 
         ZAP   DLPSAMNT,=P'0'      CLEAR AMOUNT                                 
         DROP  R4                                                               
*                                                                               
IN10E    ZIC   R3,DLPSLEN                                                       
         AR    R8,R3                                                            
*                                                                               
         CLC   CRNUM+1(2),=C'SK'                                                
         BE    IN11                                                             
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    IN10G               NO                                           
*                                                                               
         USING EXCELD,R4                                                        
         USING TRSDESCD,R8                                                      
IN10F    LA    R4,EXCWORK                                                       
         MVI   TRSDEL,TRSDELQ                                                   
         MVC   TRSDLEN,=YL1(L'ACKEYACC-1+2)                                     
         MVC   TRSDACCS(L'ACKEYACC-1),EXCSEAC+1                                 
         SR    R3,R3                                                            
         IC    R3,TRSDLEN                                                       
         AR    R8,R3                                                            
         DROP  R4,R8                                                            
*                                                                               
         USING TRCASHD,R8                                                       
IN10G    MVC   TRCSEL(3),=X'5009C7'                                             
         ZAP   TRCSAMNT,=P'0'      MAKE A GROSS MEMO ITEM OF ZERO ON SI         
         IC    R3,TRCSLEN                                                       
         AR    R8,R3                                                            
         DROP  R8                                                               
*                                                                               
         USING ACMTD,R8                                                         
         XC    0(ACMTLNQ,R8),0(R8)                                              
         MVI   ACMTEL,ACMTELQ      ADD MEDIA TRANSFER FOR TO SI POSTING         
         MVI   ACMTLEN,ACMTLNQ                                                  
         MVI   ACMTSYS,C'J'                                                     
         MVC   ACMTMED,JOBNUM+9      MEDIA CODE                                 
         MVC   ACMTCLI(12),JOBNUM+3  CLI/PRD/JOB                                
         MVC   ACMTMOS,PMOS                                                     
         MVC   ACMTDSCP,JOBNAME                                                 
         ZAP   DUB,INCAMT                                                       
         CVB   R0,DUB                                                           
         STCM  R0,15,ACMTCOM         INCOME (COMMISSION)                        
         STCM  R0,15,ACMTINTL        INCOME (INTERNAL)                          
         IC    R3,ACMTLEN                                                       
         AR    R8,R3                                                            
         DROP  R8                                                               
*                                                                               
         USING DLPOSTD,R8                                                       
IN11     TM    ACOPSTAT,ACOXJOB                                                 
         BZ    *+14                                                             
         MVC   0(L'TRSELEM,R8),TRSELEM                                          
         LA    R8,L'TRSELEM(R8)                                                 
*                                                                               
         MVC   DLPSEL(2),=X'6A71'  CREDIT INCOME                                
         ZAP   DLPSAMNT,INCAMT                                                  
         MVC   DLPSCRAC,CRNUM                                                   
         MVC   DLPSCRNM,CRNAME                                                  
         LA    RF,PROFILE                                                       
         MVC   DLPSANAL,SPACES                                                  
         MVI   DLPSTYPE,0                                                       
         MVC   DLPSANAL,OFFICE                                                  
         MVC   DLPSDBAC,PRODNUM                                                 
         MVC   DLPSDBNM,PRODNAM                                                 
         CLC   CRNUM+1(2),=C'SK'                                                
         BNE   IN12                                                             
         MVC   DLPSDBAC,JOBNUM     MAKE JOB CONTRA A/C FOR 'SK' POSTING         
         MVC   DLPSDBNM,JOBNAME                                                 
*                                                                               
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    IN12                NO                                           
*                                                                               
         SR    R3,R3               YES, DEBIT THE SK ACCOUNT NOW                
         IC    R3,DLPSLEN                                                       
         BCTR  R3,0                                                             
         LR    RF,R8                                                            
         LA    R8,1(R3,R8)                                                      
         MVC   0(L'TRSELEM,R8),TRSELEM  ADD TRANS STATUS ELEM                   
         LA    R8,L'TRSELEM(R8)                                                 
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(RF)                                                    
         MVI   DLPSEL,X'69'                                                     
         MVC   DLPSCRAC,JOBNUM                                                  
         MVC   DLPSCRNM,JOBNAME                                                 
         MVC   DLPSDBAC,CRNUM                                                   
         MVC   DLPSDBNM,CRNAME                                                  
*                                                                               
IN12     DS    0H                                                               
*&&UK                                                                           
         CLI   TWAACCS,C'*'        MUST BE A MATCH IF                           
         BNE   IN13                IT'S AN OFFICE LOGON                         
         CLC   INTFOF,TWAACCS+1                                                 
         BE    IN13                                                             
         LA    R2,INTFOFH                                                       
         MVI   ERRNUM,SECLOCK                                                   
         B     ERROR                                                            
*&&                                                                             
*                                                                               
IN13     DS    0H                                                               
         ZIC   R3,DLPSLEN                                                       
         BCTR  R3,0                                                             
         TM    COMPSTAT,X'10'      COSTING REQUIRED                             
         BZ    IN16                NO, SKIP THIS                                
*                                                                               
         CLC   CRNUM+1(2),=C'SK'   YES, IS CREDIT 'SK'                          
         BE    IN16                YES, SKIP THIS                               
*                                                                               
IN14     LR    RF,R8                                                            
         LA    R8,1(R3,R8)                                                      
         TM    ACOPSTAT,ACOXJOB                                                 
         BZ    *+14                                                             
         MVC   0(L'TRSELEM,R8),TRSELEM                                          
         LA    R8,L'TRSELEM(R8)                                                 
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(RF)                                                    
         MVI   DLPSEL,X'68'        DEBIT & CREDIT                               
         OI    DLPSTYPE,X'80'      SUBSIDIARY                                   
         MVC   DLPSDBAC,CPJNUM                                                  
         MVC   DLPSDBNM,CPJNAME                                                 
         MVC   DLPSCRAC,CNUM       12 ACCOUNT                                   
         MVC   DLPSCRNM,CNAME                                                   
*                                                                               
IN16     LA    R3,1(R3)                                                         
         AR    R8,R3                                                            
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    IN18                                                             
         CLC   CRNUM+1(2),=C'SI'   IS THIS AN SI ACCOUNT ?                      
         BE    IN17                YES, FINISH UP                               
         MVI   CRNUM+2,C'I'        NO, SET IT TO BE                             
         B     IN10F               FINISH REST OF POSTINGS                      
*                                                                               
         USING EXCELD,R4                                                        
         USING GOXBLOCK,R3                                                      
IN17     L     R3,AGOXBLK                                                       
         LA    R4,EXCWORK                                                       
         MVI   EXCACT,EXCAPST      POSTING MODE                                 
         LA    RF,INTAOFH          A(ANALYSIS OFFICE HEADER)                    
         ST    RF,EXCAANO                                                       
         LA    RF,INTDEPH          A(DEPARTMENT HEADER)                         
         ST    RF,EXCADEP                                                       
         LA    RF,INTSTFH          A(STAFF HEADER)                              
         ST    RF,EXCASTF                                                       
         LA    RF,INTEXPH          A(EXPENSE ACCOUNT HEADER)                    
         ST    RF,EXCAEXP                                                       
         MVC   EXCSECAC,CRNUM      INCOME ACCOUNT NUMBER                        
         MVC   EXCSECAN,CRNAME     INCOME ACCOUNT NAME                          
         ST    R9,EXCAGWS          A(GLOBAL WORKING STORAGE)                    
         MVC   EXCFINO,OFFICE      FINANCIAL OFFICE                             
         ZAP   EXCAMNT,INCAMT      AMOUNT                                       
         ST    R8,EXCADAY                                                       
         GOTO1 VEXCEL,EXCELD                                                    
         BNE   CURSIT                                                           
         L     R8,EXCADAY          GET UPDATED ACCDAY POINTER                   
*                                                                               
IN18     MVI   0(R8),0             END OF RECORD                                
         LA    RF,IOAREA-1                                                      
         SR    R8,RF                                                            
         STH   R8,HALF                                                          
         MVC   IOAREA(2),HALF                                                   
         BAS   RE,PUTDAY           ADD TO ACCDAY                                
*                                                                               
         SR    R3,R3                                                            
         XC    WORK,WORK                                                        
         IC    R3,INTDOCH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),INTDOC      REF                                          
         L     R3,DMCB+8                                                        
         MVC   WORK+10(4),0(R3)    DISK ADDRESS                                 
         BAS   RE,ADTWA1                                                        
         LA    R2,INTDOCH                                                       
         MVI   ERRNUM,X'FF'                                                     
         B     EXIT                                                             
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
* SUB-ROUTINE TO MOVE OUT DISPLAY DATA                                          
*                                                                               
MOVEFLD  ST    RE,SVRE                                                          
         ST    R3,SVR3                                                          
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
         LA    R3,8(R2,R1)         GET LAST BYTE OF FIELD                       
         LA    RF,1(R1)            GET LENGTH OF FIELD                          
         CLI   0(R3),C' '          LOOK FOR SIGNIFICANT DATA                    
         BH    *+10                                                             
         BCTR  R3,0                                                             
         BCT   RF,*-10                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
MOVEFLDX L     R3,SVR3                                                          
         L     RE,SVRE                                                          
         BR    RE                                                               
         EJECT                                                                  
CLEARX   NTR1                                                                   
         MVC   FLD,SPACES                                                       
         LA    R2,INTEXPH                                                       
         TM    4(R2),X'80'         HAS ACCOUNT BEEN ENTERED ?                   
         BO    *+12                YES, USE IT                                  
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
         LA    R2,INTEXPNH                                                      
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
         LA    R2,INTFOFNH                                                      
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
         LA    R2,INTAOFH                                                       
         TM    4(R2),X'80'         HAS ACCOUNT BEEN ENTERED ?                   
         BO    *+12                YES, USE IT                                  
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
         LA    R2,INTDEPH                                                       
         TM    4(R2),X'80'         HAS ACCOUNT BEEN ENTERED ?                   
         BO    *+12                YES, USE IT                                  
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
         LA    R2,INTSTFH                                                       
         TM    4(R2),X'80'         HAS ACCOUNT BEEN ENTERED ?                   
         BO    *+12                YES, USE IT                                  
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
*                                                                               
         LA    R2,INTSTFNH                                                      
         BAS   RE,MOVEFLD                                                       
         STC   RF,5(R2)                                                         
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*              RE-READ THE PROFILES TO VERIFY 1C ACCOUNT              *         
***********************************************************************         
*                                                                               
CHK1C    NTR1                                                                   
         MVC   SVKEY,KEY                                                        
         USING ACPROFD,R3                                                       
         LA    R3,CLIPROF                                                       
         MVC   SVCLI1C,ACPRCOST                                                 
         LA    R3,PRODPROF                                                      
         MVC   SVPRO1C,ACPRCOST                                                 
         DROP  R3                                                               
*                                                                               
         LA    R2,INTCLIH          RE-READ CLIENT, PRODUCT AND JOB              
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         SR    R3,R3                                                            
         IC    R3,INTCLIH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),INTCLI                                                  
*                                                                               
         LA    R8,COMPEL                                                        
         USING ACCOMPD,R8                                                       
         MVC   KEY+1(2),ACMPJOB    PRODUCTION LEDGER                            
         DROP  R8                                                               
*                                                                               
         XC    CLIPROF,CLIPROF                                                  
         XC    PRODPROF,PRODPROF                                                
         XC    JOBPROF,JOBPROF                                                  
         LA    R6,CLIPROF                                                       
         BAS   RE,GETACC                                                        
*                                                                               
         LA    R2,INTPROH                                                       
         XC    PRODPROF,PRODPROF                                                
         XC    JOBPROF,JOBPROF                                                  
*                                                                               
         SR    R3,R3                                                            
         LA    R1,KEY+3                                                         
         IC    R3,CLILNGTH         LEVA LENGTH                                  
         AR    R1,R3               READY FOR PRODUCT                            
         IC    R3,INTPROH+5                                                     
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),INTPRO                                                   
         LA    R6,PRODPROF                                                      
         BAS   RE,GETACC                                                        
*                                                                               
         LA    R2,INTJOBH                                                       
*                                                                               
         SR    R3,R3                                                            
         LA    R1,KEY+3                                                         
         IC    R3,PRDLNGTH         LEVB LENGTH                                  
         AR    R1,R3                                                            
         IC    R3,INTJOBH+5                                                     
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),INTJOB                                                   
         XC    JOBPROF,JOBPROF                                                  
         LA    R6,JOBPROF                                                       
         BAS   RE,GETACC                                                        
*                                                                               
         BAS   RE,PROFMERG                                                      
*                                                                               
         USING ACPROFD,R4                                                       
         LA    R4,PROFILE                                                       
         CLC   CPJNUM,ACPRCOST     IF COST ACCOUNT CHANGED.....                 
         BE    CHK1CX              NOTIFY RONNIE DESIMEO OR                     
         DC    H'0'                JOHN DILLON                                  
         DC    C'NOTIFY RONNIE DESIMEO OR JOHN DILLON'                          
*                                                                               
CHK1CX   XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*----------------------------------------------------------------               
*        ACBATCODE                                                              
*----------------------------------------------------------------               
*                                                                               
       ++INCLUDE ACBATCODE                                                      
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------               
*        LOCAL WORKING STORAGE                                                  
*----------------------------------------------------------------               
*                                                                               
PROGD    DSECT                                                                  
SVRE     DS    A                   SAVE REGISTER 14                             
SVR3     DS    A                   SAVE REGISTER 3                              
*                                                                               
JOBNUM   DS    CL15                JOB NUMBER                                   
JOBNAME  DS    CL36                JOB NAME                                     
*                                                                               
CRNUM    DS    CL15                SI/SK ACCOUNT NUMBER                         
CRNAME   DS    CL36                SI/SK ACCOUNT NAME                           
*                                                                               
CPJNUM   DS    CL15                1C ACCOUNT NUMBER                            
CPJNAME  DS    CL36                IC ACCOUNT NAME                              
*                                                                               
CNUM     DS    CL15                12 ACCOUNT NUMBER                            
CNAME    DS    CL36                12 ACCOUNT NAME                              
*                                                                               
PRODNUM  DS    CL15                PRODUCT NUMBER                               
PRODNAM  DS    CL36                PRODUCT NAME                                 
*                                                                               
INCAMT   DS    PL6                                                              
*&&US                                                                           
         DS    0F                                                               
TAX1     DS    (STXLEN)C                                                        
         DS    0F                                                               
TAX2     DS    (STXLEN)C                                                        
WRKC     DS    CL2                 WORKCODE FOR OPTVAL/LOOKUP                   
WRKCAMT  DS    PL6                 AMOUNT FOR SAME                              
*&&                                                                             
OFFICE   DS    CL2                                                              
ELCODE   DS    CL1                                                              
SAVEDOC  DS    CL6                                                              
SAVEDATE DS    CL3                                                              
SAVEKEY  DS    CL15                                                             
BLOCK    DS    CL128                                                            
*                                                                               
TRSELEM  DS    XL(TRSLNQ)                                                       
ANOELEM  DS    XL(ANOLNQ)                                                       
*                                                                               
SVCLIH   DS    CL11                                                             
SVCLI1C  DS    CL(L'CPJNUM)                                                     
*                                                                               
SVPROH   DS    CL11                                                             
SVPRO1C  DS    CL(L'CPJNUM)                                                     
SVJOBH   DS    CL14                                                             
SVKEY    DS    CL(L'KEY)                                                        
*                                                                               
EXCWORK  DS    0D                                                               
         DS    CL(EXCELNQ)                                                      
*                                                                               
KEY      DS    CL49                                                             
IOAREA   DS    3000C                                                            
PROGDX   DS    0C                                                               
         EJECT                                                                  
*&&US                                                                           
*----------------------------------------------------------------               
*        STX DSECT                                                              
*----------------------------------------------------------------               
*                                                                               
STXD     DSECT                                                                  
STXACC   DS    CL15                                                             
STXNAM   DS    CL36                                                             
STXWK    DS    CL2                                                              
STXAMT   DS    PL6                                                              
STXLEN   EQU   *-STXD                                                           
         EJECT                                                                  
*&&                                                                             
*----------------------------------------------------------------               
*        ACBATDSECT                                                             
*----------------------------------------------------------------               
*                                                                               
       ++INCLUDE ACBATDSECT                                                     
         EJECT                                                                  
*----------------------------------------------------------------               
*        ACBATF7D                                                               
*----------------------------------------------------------------               
*                                                                               
       ++INCLUDE ACBATF7D                                                       
         ORG   TWAHOLE                                                          
COSTAC   DS    CL12                                                             
JOBSTAT  DS    X                   SAVE AREA FOR ACOPSTAT                       
         EJECT                                                                  
*ACEXCELD                                                                       
       ++INCLUDE ACEXCELD                                                       
         EJECT                                                                  
*----------------------------------------------------------------               
*        OTHER INCLUDES                                                         
*----------------------------------------------------------------               
*                                                                               
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
**PAN#1  DC    CL21'028ACBAT08   08/10/10'                                      
         END                                                                    
