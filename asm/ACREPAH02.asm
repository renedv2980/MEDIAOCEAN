*          DATA SET ACREPAH02  AT LEVEL 099 AS OF 07/23/13                      
*PHASE ACAH02C                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE QSORT                                                                  
*INCLUDE UNDERLIN                                                               
         TITLE 'MARK CHARGES AS HELD SO JOB CAN BE BILLED'                      
ACAH02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACAH**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACAHD,RC                                                         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   REQF                                                             
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         L     RF,ADMASTD                                                       
         USING MASTD,RF                                                         
         L     RF,MCBXAREA                                                      
         ST    RF,ADBOX                                                         
*                                                                               
         L     RF,=A(HDHOOK)                                                    
         ST    RF,HEADHOOK                                                      
*                                                                               
         L     RF,=A(SAVERC)                                                    
         ST    RC,0(RF)                                                         
*                                                                               
         BAS   RE,GETBUFF                                                       
         DROP  RF                                                               
*                                                                               
REQF     CLI   MODE,REQFRST                                                     
         BNE   LEVA                                                             
         MVC   PAGE,=H'1'                                                       
         GOTO1 DATCON,DMCB,(5,0),(1,TODAY3)                                     
*                                                                               
         XC    UNHOLDTP,UNHOLDTP                                                
         CLI   QOPT1,C'A'                                                       
         BNE   *+8                                                              
         MVI   UNHOLDTP,GDATAHLD   UNHOLD HELD BY AUTOHOLD                      
         CLI   QOPT1,C'M'                                                       
         BNE   *+8                                                              
         MVI   UNHOLDTP,GDATMHLD   UNHOLD HELD BY MARKER                        
         CLI   QOPT1,C'B'                                                       
         BNE   *+8                                                              
         MVI   UNHOLDTP,GDATBHLD   UNHOLD HELD BY BILLING                       
*                                                                               
         XC    UNHOLDDT,UNHOLDDT                                                
         CLC   QSTART,SPACES                                                    
         BE    REQF30                                                           
         GOTO1 DATCON,DMCB,(0,QSTART),(1,UNHSTART)                              
*                                                                               
REQF30   CLC   QEND,SPACES                                                      
         BE    REQF40                                                           
         GOTO1 DATCON,DMCB,(0,QEND),(1,UNHEND)                                  
*                                                                               
REQF40   ZAP   RUNCNT,=P'0'                RECORD CHANGED CNT TO O              
         MVC   RUNTYPE,=C'DRAFT RUN'                                            
         MVI   RCWRITE,C'N'                                                     
         CLI   QOPT2,C'Y'                                                       
         BNE   *+14                                                             
         MVC   RUNTYPE,=C'LIVE RUN '                                            
         MVI   RCWRITE,C'Y'                                                     
*                                                                               
         MVC   TITLE,=C'  MARK TRANSACTIONS AS HELD   '                         
         MVC   TITLE2,=C'  -------------------------   '                        
         CLI   QOPT1,C' '                                                       
         BE    REQF60                                                           
*                                                                               
         MVC   TITLE,=C'  UNHOLD ALL TRANSACTIONS     '                         
         MVC   TITLE2,=C'  -----------------------     '                        
         CLI   QOPT1,C'A'                                                       
         BNE   *+16                                                             
         MVC   TITLE,=C'  UNHOLD AAH TRANSACTIONS     '                         
         MVC   TITLE2,=C'  -----------------------     '                        
*                                                                               
         CLI   QOPT1,C'M'                                                       
         BNE   *+16                                                             
         MVC   TITLE,=C'UNHOLD INV/MARKER TRANSACTIONS'                         
         MVC   TITLE2,=C'------------------------------'                        
*                                                                               
         CLI   QOPT1,C'B'                                                       
         BNE   *+16                                                             
         MVC   TITLE,=CL30'UNHOLD BILLING TRANSACTIONS'                         
         MVC   TITLE2,=CL30'--------------------------'                         
*                                                                               
REQF60   L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
         GOTO1 ACMAJOBL,DMCB,FLDH,ACMACOLL,ADCOMFAC                             
         CLI   DMCB+4,X'00'                                                     
         BNE   XIT                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
         EJECT                                                                  
*-------------------------------------------------------------+                 
*        SAVE ACCOUNT NAMES                                                     
*-------------------------------------------------------------+                 
         USING ACTRECD,R4          SAVE JOB NAME                                
LEVA     CLI   MODE,PROCLEVA                                                    
         BNE   LEVB                                                             
         L     R4,ADHEIRA                                                       
         MVC   CLICODE,SPACES                                                   
         MVC   CLICODE(3),ACTKACT                                               
         L     R4,ADLVANAM                                                      
         LA    R5,CLINAME                                                       
         BAS   RE,GETNAME                                                       
         B     XIT                                                              
*                                                                               
*                                                                               
LEVB     CLI   MODE,PROCLEVB                                                    
         BNE   ACCOUNT                                                          
         L     R4,ADHEIRB                                                       
         MVC   PROCODE,SPACES                                                   
         MVC   PROCODE(3),ACTKACT+3                                             
         L     R4,ADLVBNAM                                                      
         LA    R5,PRONAME                                                       
         BAS   RE,GETNAME                                                       
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------+                 
*        DETERMINE IF AN ACCOUNT IS ELIGIBLE FOR THIS REPORT                    
*-------------------------------------------------------------+                 
ACCOUNT  CLI   MODE,PROCACC                                                     
         BNE   HIST                                                             
         MVI   FCRDTRNS,C'N'       ASSUME JOB WILL BE REJECTED                  
         MVI   RCSUBPRG,0                                                       
         MVI   FORCEMID,C'N'                                                    
         XC    JOBSTAT,JOBSTAT                                                  
         XC    LAST,LAST                                                        
         BAS   RE,ZAPACCS          CLEAR ACCOUNT BUCKETS                        
*                                                                               
         USING RSTELD,R5                                                        
         L     R5,ADACCSTA                                                      
         TM    RSTSTAT,RSTSACIC    ACCOUNT IS CLOSED                            
         BO    XIT                 YES                                          
*                                                                               
         CLI   QOPT3,C' '          LOCKED OPTION                                
         BE    ACC20                                                            
         CLI   QOPT3,C'L'          LOCKED OPTION                                
         BNE   ACC10                                                            
         TM    RSTSTAT,RSTSACIL                                                 
         BNO   XIT                                                              
*                                                                               
ACC10    CLI   QOPT3,C'S'          SUPRESS LOCKED                               
         BNE   ACC20                                                            
         TM    RSTSTAT,RSTSACIL                                                 
         BO    XIT                                                              
*                                                                               
         USING GOBLOCKD,R6                                                      
ACC20    L     R6,ADGOBLOC                                                      
         CLI   GOBILTYP,C'C'       CLIENT BILLING?                              
         BE    XIT                                                              
         DROP  R6                                                               
*                                                                               
         BAS   RE,LOOKUP           CALL JOBBER FOR ESTIMATE DATA                
*                                                                               
         USING JBLOCKD,R5                                                       
         CLI   JBNEWEST,JBMCSQ     IS THIS A BO JOB?                            
         BNE   ACC30               NO                                           
         USING MJETABD,R3                                                       
         ZAP   CUREST,MJETVAL      YES, GET NET ESTIMATE                        
         ZAP   HIREV,MJETVAL+6(6)  AND HIGHEST REVISION                         
         B     ACC40                                                            
         DROP  R3                                                               
*                                                                               
         USING JBCOLD,R3                                                        
ACC30    ZAP   CUREST,JBCOLVAL     GET NET ESTIMATE                             
         ZAP   HIREV,JBCOLVAL+6(6) GET HIGHEST REVISION                         
*                                                                               
ACC40    MVC   CUREST#,JBCURVER    SAVE ESTIMATE NUMBERS                        
         MVC   HIREV#,JBHIREV                                                   
         DROP  R3                                                               
*                                                                               
ACC45    CP    CUREST,=P'0'        ANY ESTIMATE                                 
         BE    *+8                 NO                                           
         OI    JOBSTAT,HASANEST                                                 
*                                                                               
         USING GOBLOCKD,R6                                                      
         L     R6,ADGOBLOC                                                      
         CLI   GONEEDES,C'Y'       DO WE NEED AN ESTIMATE ?                     
         BNE   *+8                 NO                                           
         OI    JOBSTAT,NEEDSEST                                                 
*                                                                               
         USING ABLELD,R2                                                        
         L     R2,ADACCBAL                                                      
         ZAP   ACTUAL,ABLDR        SAVE TOTAL DR'S AS ACTUALS                   
         ZAP   BILLED,ABLCR        SAVE TOTAL CR'S AS BILLED                    
*                                                                               
         CP    ACTUAL,=P'0'        ANY DEBITS ON JOB                            
         BE    *+8                 NO, NOTHING TO HOLD                          
         OI    JOBSTAT,HASDRS                                                   
*                                                                               
         CP    ABLDR,ABLCR         IS ACCOUNT FULLY BILLED?                     
         BE    *+8                 YES                                          
         OI    JOBSTAT,NOTBILED                                                 
*                                                                               
         ZAP   OVEREST,=P'0'                                                    
         OC    GOOVRPER,GOOVRPER   MAXIMUM OVER PERCENT                         
         BZ    ACC50                                                            
         ZAP   PL13,GOOVRPER                                                    
         MP    PL13,CUREST         PERCENT OF EST X EST.                        
         DP    PL13,=PL3'10000'                                                 
         ZAP   OVEREST,PL13(10)                                                 
         SP    OVEREST,CUREST      SUBTRACT OUT EST AMOUNT LEAVING              
*                                  ALLOWABLE OVER                               
ACC50    ZAP   OVERAMT,=P'0'                                                    
         ZAP   MAXAMT,OVEREST                                                   
         CP    GOOVRAMT,=P'0'      IS THERE A MAX OVER AMOUNT                   
         BE    ACC55               NO, USE OVER EST                             
*                                                                               
         ZAP   OVERAMT,GOOVRAMT                                                 
         CP    OVEREST,OVERAMT     TAKE THE LESS OF THESE TWO                   
         BL    *+10                                                             
         ZAP   MAXAMT,OVERAMT                                                   
*                                                                               
ACC55    ZAP   MAXEST,CUREST       MAX ESTIMATE AMOUNT FOR BILLING              
         AP    MAXEST,MAXAMT                                                    
*                                                                               
         ZAP   HOLDTOBL,ACTUAL     ACTUALS                                      
         SP    HOLDTOBL,MAXEST     LESS MAX OVER EST TO BILL                    
         BM    *+8                 IF NOT POSITIVE, I STILL CAN BILL            
         OI    JOBSTAT,OVEST                                                    
*                                                                               
         CLI   GOBILTYP,C'P'       PROGRESSIVE?                                 
         BE    ACC60               Y                                            
*                                                                               
         CLI   GOBILTYP,C'1'       ONE LINE TOTAL                               
         BE    ACC60               Y                                            
*                                                                               
         CLI   GOBILTYP,C'T'       TOTAL                                        
         BNE   *+8                 N                                            
ACC60    OI    JOBSTAT,T_1_OR_P                                                 
*                                                                               
         CLI   QOPT1,C' '          UNHOLD RUN?                                  
         BNE   ACC80               YES, LOOK AT ALL JOBS                        
*                                                                               
         TM    JOBSTAT,HASANEST+NEEDSEST+HASDRS+NOTBILED+OVEST+T_1_OR_P         
         BNO   XIT                 JOB IS NOT HOLDABLE                          
*                                                                               
ACC80    MVI   FCRDTRNS,C'Y'                                                    
         XC    TRNCNT,TRNCNT       INITIALIZE                                   
         MVC   NAMENEXT,ANAMES     CONTA ACCOUNT NAME TABLE                     
*                                                                               
         USING BIND,R5                                                          
         L     R5,ACACLST                                                       
         XC    BININ,BININ         RESEST CONTRA ACCOUNT BINSRCH TABLE          
         DROP  R5                                                               
*                                                                               
         ZAP   SORTAMNT,=P'0'                                                   
         USING ACTRECD,R4          SAVE JOB NAME                                
         L     R4,ADACC                                                         
         MVC   JOBCODE(6),ACTKACT+6                                             
         L     R4,ADACCNAM                                                      
         LA    R5,JOBNAME                                                       
         BAS   RE,GETNAME                                                       
*                                                                               
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
HIST     CLI   MODE,PROCSBAC                                                    
         BNE   TRNS                                                             
         USING CHDRECD,R4          SAVE CONTRA ACCOUNT NAME                     
         L     R4,ADSUBAC                                                       
         USING LISTD,R2                                                         
         LA    R2,LISTDATA                                                      
         MVC   LISTKEY,CHDKCUNT    SAVE U/L/ACCOUNT                             
         MVI   ELCODE,CACELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CACELD,R4           BUILD A DUMMY 20 ELEMENT                     
         XC    ELEMENT,ELEMENT                                                  
         ZIC   R1,CACLN                                                         
         SH    R1,=Y(CACLN1Q)                                                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT+2(0),CACNAME                                             
         MVI   ELEMENT,X'20'                                                    
         LA    R1,3(R1)                                                         
         STC   R1,ELEMENT+1                                                     
         LA    R4,ELEMENT                                                       
*                                                                               
         L     R3,ACACLST                                                       
         BAS   RE,PUTACNM          SAVE CONTRA ACCOUNT NAME                     
         BE    *+6                 NAME FITS IN TABLE                           
         DC    H'0'                                                             
         B     XIT                                                              
         DROP  R2,R4                                                            
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        PUT ELIGIBLE TRANSACTIONS TO QSORT TABLE                               
*----------------------------------------------------------------------         
*                                                                               
TRNS     CLI   MODE,PROCTRNS                                                    
         BNE   ACCOUNTX                                                         
*                                                                               
         L     R4,ADTRANS                                                       
         USING TRNELD,R4                                                        
         CLI   0(R4),TRNELQ                                                     
         BNE   XIT                                                              
*                                                                               
         TM    TRNSTAT,TRNSDR      DEBITS ONLY                                  
         BNO   TRN100              SAVE BILLING TDATE THOUGH                    
*                                                                               
         TM    TRNSTAT,TRNSREV     NO OFFSETS                                   
         BO    XIT                 SAVE BILLING TDATE                           
*                                                                               
         LR    R5,R4                                                            
         SH    R5,DATADISP         GET KEY                                      
         USING TRNRECD,R5                                                       
         CLC   TRNKWORK,=C'**'     IS THIS A P/O                                
         BE    XIT                 YES                                          
*                                                                               
         CLI   QOPT1,C' '          IS THIS AN UNHOLD RUN                        
         BNE   TRN50               YES, CAN UNHOLD IF BILLED/ALLOCATED          
*                                                                               
         OC    ACCOUSED(2,R5),ACCOUSED(R5)                                      
         BNZ   TRNX                                                             
*                                                                               
         USING ACMD,R4                                                          
         L     R4,AMONACC                                                       
         L     R4,ACMAPRO2                                                      
         CLI   0(R4),PTAELQ                                                     
         B     TRN40+8                                                          
*                                                                               
         USING PTAELD,R4                                                        
TRN40    MVI   ELCODE,PTAELQ                                                    
         BAS   RE,NEXTEL                                                        
         BNE   TRN50                                                            
*                                                                               
         CLI   PTATYPE,0           IS THIS A BLANK ELEMENT?                     
         BE    TRN40               YES  LOOK AGAIN                              
         B     TRNX                NO, CAN'T HOLD                               
*                                                                               
         USING TRNELD,R4                                                        
TRN50    L     R4,ADTRANS                                                       
         TM    TRNSTAT,TRNSHOLD    IS THIS TRAN HELD                            
         BNO   *+10                                                             
         AP    HELD,TRNAMNT        SAVE TOTAL HELD ON JOB                       
*                                                                               
         AP    BILLABLE,TRNAMNT    SAVE AS BILLBVLE                             
*                                                                               
         CLI   QOPT1,C' '          IS THIS AN UNHOLD RUN                        
         BE    TRN90               NO                                           
*                                                                               
         TM    TRNSTAT,TRNSHOLD    IS THIS TRAN HELD                            
         BNO   TRNX                NO, NOT ELIGIBLE TO BE UNHELD                
*                                                                               
         L     R4,ADTRANS                                                       
         BAS   RE,CHKTRAN                                                       
*                                                                               
         BNE   TRN90               REJECTED BY CHKTRAN                          
*                                                                               
TRN70    L     R4,ADTRANS                                                       
         USING TRNELD,R4                                                        
         AP    TOBEHELD,TRNAMNT    SAVE UNHELD AMOUNT HERE                      
*                                                                               
TRN90    L     R4,ADTRANS                                                       
         AP    SORTAMNT,TRNAMNT    KEEP TOTAL AMOUNT PUT TO SORT                
*                                                                               
         BAS   RE,PUTSORT          PUT TRAN TO SORT                             
*                                                                               
         B     TRNX                                                             
*                                                                               
         USING TRNELD,R4                                                        
TRN100   L     R4,ADTRANS                                                       
         CLC   TRNANAL,=C'99'      MAKE SURE THIS IS BILLING                    
         BNE   TRNX                                                             
         CLC   TRNDATE,LAST        KEEP HIGHEST BILLING T-DATE                  
         BNH   TRNX                                                             
         MVC   LAST,TRNDATE                                                     
*                                                                               
TRNX     EQU   *                                                                
         B     XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
*----------------------------------------------------------------------         
*        MARK SELECTED TRANSACTIONS, PRODUCE REPORTS                            
*----------------------------------------------------------------------         
*                                                                               
ACCOUNTX CLI   MODE,ACCLAST                                                     
         BNE   RUNL                                                             
*                                                                               
         CLI   FCRDTRNS,C'Y'       DID I READ TRANSACTIONS                      
         BNE   XIT                 NO, JOB WAS REJECTED                         
*                                                                               
         OC    TRNCNT,TRNCNT       ANY TRANSACTIONS MAKE IT TO QSORT            
         BZ    XIT                 NO                                           
*                                                                               
*        BAS   RE,SAVESEQ                                                       
         CLI   QOPT1,C' '          IS THIS AN UNHOLD RUN                        
         BE    ACL10               NO                                           
*                                                                               
         CP    TOBEHELD,=P'0'      IS THERE ANYTHING TO UNHOLD                  
         BE    XIT                 NO                                           
*                                                                               
         BAS   RE,UNHOLD           UNHOLD THE TRANSACTIONS IN ASORT             
*        BAS   RE,RESTSEQ          RESTORE READ SEQUENCE                        
         B     XIT                 AND LEAVE                                    
*                                                                               
ACL10    ZAP   TOBEHELD,HOLDTOBL   AMOUNT NEEDED TO BILL                        
         SP    TOBEHELD,HELD       LESS PREV HELD                               
         BNP   XIT                                                              
*                                                                               
         BAS   RE,ACCREPT          PRODUCE ACCOUNT TOTALS                       
*                                                                               
         ZAP   PKWORK,TOBEHELD                                                  
*                                                                               
         BAS   RE,SORTAMT          SORT BY AMOUNT                               
         L     R2,ASORT                                                         
         LH    R3,TRNCNT                                                        
*                                                                               
         USING TSORTD,R2                                                        
ACL20    CP    PKWORK,=P'0'        NEED TO HOLD TO BILL?                        
         BNH   ACL70               NO                                           
*                                                                               
         CLI   TSSIGN,NEGATIVE     NEGATIVE                                     
         BE    ACL50               YES, DON'T HOLD                              
*                                                                               
         TM    TSSTAT,TSHELD       HELD ALREADY                                 
         BO    ACL50               YES, DON'T HOLD                              
*                                                                               
         SP    PKWORK,TSKAMT                                                    
*                                                                               
         BAS   RE,HOLDTRN                                                       
         OI    TSSTAT,TSHELD       FLAD AS HELD                                 
         MVC   TSHDATE,TODAY3      SET HELD DATE FOR REPORT                     
         MVI   TSHTYPE,C'A'                                                     
*                                                                               
         BAS   RE,HOLDINV          HOLD TRANSACTIONS W/ SAME NUM/T DATE         
*                                                                               
ACL50    LA    R2,TSORTLN(R2)                                                   
         BCT   R3,ACL20                                                         
*                                                                               
ACL70    EQU   *                   PRODUCE REPORT                               
         BAS   RE,SORTSEQ          SORT BY SEQUENCE NUMBER                      
*                                                                               
         BAS   RE,TRNREPT          AND REPORT                                   
*                                                                               
*        BAS   RE,RESTSEQ          RESTORE READ SEQUENCE                        
*                                                                               
ACLX     B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        RELEASE GETMAINED SPACE                                                
*----------------------------------------------------------------------         
RUNL     CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         BAS   RE,RELBUFF          RELEASE GETMAINED SPACE                      
         B     XIT                                                              
*                                                                               
*        END OF MODE DEPENDENT SUB ROUTINES                                     
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        GET ESTIMATE DATA                                                      
*----------------------------------------------------------------------         
LOOKUP   NTR1                                                                   
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
*                                                                               
         L     R5,ACMAJOBB                                                      
         USING JBLOCKD,R5                                                       
         MVC   JBAJOB,ADACC                                                     
         MVC   JBACOLS,ACMACOLL                                                 
         MVC   JBACOM,ADCOMFAC                                                  
         MVC   JBAGOBLK,ADGOBLOC                                                
         MVC   JBAIO,ACMAJOBI                                                   
         MVC   JBAKEY,LASTIO                                                    
         MVC   JBGETOPT,GETOPT                                                  
*                                                                               
         MVC   JBACOLTB,ACMACOL                                                 
         MVC   JBLCOLTB,ACMLCOL                                                 
         MVC   JBAOPVTB,ACMAOPV                                                 
         MVC   JBLOPVTB,ACMLOPV                                                 
         MVI   JBSELFUN,JBGETDE    GET BO DETAILS                               
*                                                                               
         LA    RE,FLDH                                                          
         ST    RE,JBORICLI                                                      
*                                                                               
         GOTO1 ACMAJOBR,DMCB,ACMAJOBB                                           
         CLI   JBERROR,X'00'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,ACMACOL                                                       
         XIT1  REGS=(R3,R5)                                                     
*                                                                               
FLDH     DC    AL1(8+L'FLD),4X'00',AL1(L'FLD),AL2(0)                            
FLD      DC    C'CE,HR'                                                         
*                                                                               
         DROP  R3,R5                                                            
         EJECT                                                                  
ACCREPT  NTR1                                                                   
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   FORCEMID,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
*                                                                               
         BAS   RE,JOBDATA          GET THE JOB LEVEL DATA                       
*                                                                               
         LA    R0,ACCACCNM         PRINT THE ACCOUNT LEVEL ACCUMS               
         LA    R2,ACCACCS                                                       
         LA    R4,PACCSCON                                                      
         USING PACCD,R5                                                         
         LA    R5,P+1                                                           
*                                                                               
ACCR20   MVC   *+8(2),0(R4)                                                     
         LA    R6,HALF             NOTE, HALF IS A DUMMY                        
*                                                                               
         EDIT  (P6,0(R2)),(13,0(R6)),2,MINUS=YES,ZERO=NOBLANK                   
         LA    R2,6(R2)            NEXT ACCUMULATOR                             
         LA    R4,2(R4)            BUMP SCON TABLE                              
         BCT   R0,ACCR20                                                        
*                                                                               
         BAS   RE,PRINTEM                                                       
         BAS   RE,PRINTEM                                                       
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        UNHOLD SELECTED TRANSACTIONS IN ASORT                                  
*----------------------------------------------------------------------         
         USING TSORTD,R2                                                        
UNHOLD   NTR1                                                                   
         BAS   RE,ACCREPT          PRODUCE ACCOUNT TOTALS                       
         BAS   RE,SORTSEQ                                                       
         L     R2,ASORT                                                         
         LH    R5,TRNCNT           NUMBER OF TRANSACTIONS IN ASORT              
*                                                                               
UNH10    TM    TSSTAT,TSHELD       IS THIS TRAN HELD                            
         BNO   UNH50               NO GET NEXT                                  
*                                                                               
         LA    R3,TSTRNKEY         GET THIS TRANSACTION FROM FILE               
         BAS   RE,READUP                                                        
*                                                                               
         L     R4,AMYIO                                                         
         AH    R4,DATADISP                                                      
         BAS   RE,CHKTRAN                                                       
         BNE   UNH50               TRAN REJECTED                                
*                                                                               
         BAS   RE,UNHOLDTR         UNHOLD THIS TRANSACTION                      
*                                                                               
UNH50    LA    R2,TSORTLN(R2)      AND GET NEXT                                 
         BCT   R5,UNH10                                                         
*                                                                               
         L     R3,ADACC            RE-ESTABLISH KEY SEQ                         
         BAS   RE,READ                                                          
*                                                                               
         BAS   RE,TRNREPT          AND REPORT                                   
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        HOLD THE TRANSACTION KEYED BY THE SORT RECORD AT 0(R2)                 
*----------------------------------------------------------------------         
         USING TSORTD,R2                                                        
HOLDTRN  NTR1                                                                   
*                                                                               
         LA    R3,TSTRNKEY                                                      
         BAS   RE,READUP           GET THIS TRAN IN MYIO                        
*                                                                               
         USING TRNELD,R4                                                        
         L     R4,AMYIO                                                         
         AH    R4,DATADISP                                                      
         CLI   TRNEL,TRNELQ                                                     
         BNE   XIT                                                              
         OI    TRNSTAT,TRNSHOLD                                                 
*                                                                               
         BAS   RE,PUTDATE                                                       
*                                                                               
         BAS   RE,WRITE                                                         
         OI    JOBSTAT,MARKED                                                   
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        THE TRANSACTION KEYED BY THE SORT RECORD AT 0(R2) BE HELD              
*        HOLD ANY OTHER TRANSACTIONS WHICH HAVE THE SAME                        
*        INVOICE NUMBER/TDATE                                                   
*        R2 IS CURRENT QSORT RECORD                                             
*        R3 IS COUNT                                                            
*---------------------------------------------------------------------          
         USING TSORTD,R2                                                        
HOLDINV  NTR1                                                                   
*                                                                               
         USING TRNRECD,R4                                                       
         LA    R4,TSTRNKEY                                                      
         MVC   SVREF,TRNKREF                                                    
         MVC   SVDATE,TRNKDATE                                                  
         BCTR  R3,0                IF AT END, XIT                               
         LTR   R3,R3                                                            
         BZ    HIX                                                              
*                                                                               
         LA    R2,TSORTLN(R2)      GET NEXT SORT RECORD                         
*                                                                               
HI10     LA    R4,TSTRNKEY                                                      
         CLC   SVREF,TRNKREF                                                    
         BNE   HI50                                                             
         CLC   SVDATE,TRNKDATE                                                  
         BNE   HI50                                                             
*                                                                               
         TM    TSSTAT,TSHELD       HELD ALREADY                                 
         BO    HI50                YES, DON'T HOLD                              
*                                                                               
         SP    PKWORK,TSKAMT                                                    
*                                                                               
         BAS   RE,HOLDTRN                                                       
         OI    TSSTAT,TSHELD       FLAD AS HELD                                 
         MVC   TSHDATE,TODAY3      SET HELD DATE FOR REPORT                     
         MVI   TSHTYPE,C'A'        SET TYPE TO AAH                              
*                                                                               
HI50     LA    R2,TSORTLN(R2)                                                   
         BCT   R3,HI10                                                          
*                                                                               
HIX      B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        UNHOLD THE TRANSACTION KEYED BY THE SORT RECORD AT 0(R2)               
*----------------------------------------------------------------------         
         USING TSORTD,R2                                                        
UNHOLDTR NTR1                                                                   
         LA    R3,TSTRNKEY                                                      
         BAS   RE,READUP                                                        
*                                                                               
         USING TRNELD,R4                                                        
         L     R4,AMYIO                                                         
         AH    R4,DATADISP                                                      
         CLI   TRNEL,TRNELQ                                                     
         BNE   XIT                                                              
         NI    TRNSTAT,X'FF'-TRNSHOLD                                           
*                                                                               
         BAS   RE,DELDATE          DELETE THE DATE ELEMENT                      
*                                                                               
         NI    TSSTAT,X'FF'-TSHELD FIX UP SORT RECORD                           
         XC    TSHDATE,TSHDATE                                                  
         MVI   TSHTYPE,C' '                                                     
*                                                                               
         BAS   RE,WRITE                                                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
ZAPACCS  NTR1                                                                   
*                                                                               
         LA    R0,ACCACCNM         PRINT THE ACCOUNT LEVEL ACCUMS               
         LA    R2,ACCACCS                                                       
*                                                                               
ZAPA20   ZAP   0(6,R2),=P'0'                                                    
         LA    R2,6(R2)                                                         
         BCT   R0,ZAPA20                                                        
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        ADD A HELD DATE ELEMENT TO THE RECORD IN AMYIO                         
*            DATE IS IN TODAY3                                                  
*----------------------------------------------------------------------         
PUTDATE  NTR1                                                                   
         XC    ELEMENT,ELEMENT     ADD HELD DATE ELEMENT                        
         USING GDAELD,R4           GENERAL DATE EL                              
         LA    R4,ELEMENT                                                       
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDATAHLD                                                 
         MVC   GDADATE,TODAY3                                                   
         GOTO1 ADDEL,DMCB,AMYIO,ELEMENT                                         
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        DELETE THE DATE ELEMENT ON THIS RECORD                                 
*----------------------------------------------------------------------         
DELDATE  NTR1                                                                   
         XC    ELEMENT,ELEMENT     ADD HELD DATE ELEMENT                        
         USING GDAELD,R4           GENERAL DATE EL                              
         LA    R4,ELEMENT                                                       
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         XC    DMCB,DMCB                                                        
         GOTO1 DELEL,DMCB,('GDAELQ',AMYIO)                                      
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        SEE IF I CAN UNHOLD THE TRANSACTION AT 0(R4)                           
*----------------------------------------------------------------------         
*                                                                               
CHKTRAN  NTR1                                                                   
         OC    UNHOLDDT,UNHOLDDT  UNHOLD REGARDLESS OF HELD DATE                
         BNZ   CHKT15              NO, NEED DATE                                
*                                                                               
         OC    UNHOLDTP,UNHOLDTP   UNHOLD A SPECIFIC HOLD TYPE                  
         BZ    CHKTEQ              NO, DONT NEED DATE EL                        
*                                                                               
         USING GDAELD,R4           GENERAL DATE EL                              
CHKT15   MVI   ELCODE,GDAELQ                                                    
         BAS   RE,NEXTEL                                                        
         BE    CHKT20                                                           
*                                                                               
         OC    UNHOLDDT,UNHOLDDT   DID THEY WANT A DATE FILTER?                 
         BNZ   CHKTNEQ             YES, NEED DATE TO UNHOLD                     
*                                                                               
         CLI   UNHOLDTP,GDATMHLD   UNHOLD MARKER/INV                            
         BE    CHKTEQ              YES, ACCEPT NO DATE EL AS =INV               
         B     CHKTNEQ             ELSE, REJECT T                               
*                                                                               
CHKT20   BAS   RE,CHKDATE          GOOD DATE                                    
         BNE   CHKTNEQ             NO, REJECT                                   
*                                                                               
         OC    UNHOLDTP,UNHOLDTP   UNHOLD REGARDLESS OF HOLDER                  
         BZ    CHKTEQ              YES, ACCEPT                                  
*                                                                               
         CLC   GDATYPE,UNHOLDTP    IS THIS THE TYPE WE WANT?                    
         BNE   CHKTNEQ             NO, GET NEXT                                 
*                                                                               
CHKTEQ   CR    R4,R4                                                            
         B     *+6                                                              
*                                  WANT TO UNHOLD                               
CHKTNEQ  CR    R4,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        CHECK THE DATE GDAEL IN THE ELEMENT AT 0(R4)                           
*----------------------------------------------------------------------         
         USING GDAELD,R4           GENERAL DATE EL                              
CHKDATE  EQU   *                                                                
         OC    UNHOLDDT,UNHOLDDT   ANY DATE FILTERING ON THIS REQUEST           
         BZ    CHKXEQ              NO, DATE IS OK                               
*                                                                               
         CLC   GDADATE,UNHSTART    WAS THIS HELD BEFORE                         
         BL    CHKXNEQ             YES                                          
*                                                                               
         OC    UNHEND,UNHEND       IS THERE AN END                              
         BZ    CHKXEQ              NO, I'M OK                                   
*                                                                               
         CLC   GDADATE,UNHEND      WAS THIS HELD AFTER                          
         BH    CHKXNEQ             YES                                          
*                                                                               
CHKXEQ   CR    R4,R4                                                            
         B     *+6                                                              
CHKXNEQ  CR    R4,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
TRNREPT  NTR1                                                                   
*                                                                               
         BAS   RE,PRINTEM                                                       
         BAS   RE,BXTOP                                                         
         BAS   RE,PRINTEM                                                       
         BAS   RE,SETMID                                                        
         MVI   RCSUBPRG,1                                                       
         MVI   FORCEMID,C'Y'                                                    
*                                                                               
         ZAP   CACNT,=P'0'                                                      
         ZAP   WCCNT,=P'0'                                                      
*                                                                               
         XC    PREVTRN,PREVTRN                                                  
         LA    R6,ACCBUCKS                                                      
         BAS   RE,ZAPBUCKS                                                      
*                                                                               
         L     R2,ASORT                                                         
         LH    R0,TRNCNT                                                        
         USING TSORTD,R2                                                        
         USING TRNRECD,R3                                                       
         USING PTRND,R4                                                         
         LA    R4,P                                                             
TRRPT10  LA    R3,TSTRNKEY                                                      
*                                                                               
*                                                                               
         BAS   RE,TRLAST           PRINT ANY LASTS                              
*                                                                               
         BAS   RE,TRFIRST          PRINT ANY FIRSTS YOU MIGHT NEED              
*                                                                               
         MVC   PTREF,TRNKREF                                                    
         GOTO1 DATCON,DMCB,(1,TRNKDATE),(5,PTDATE)                              
*                                                                               
         CLI   TSHTYPE,C' '                                                     
         BNH   TRRPT20                                                          
         MVC   PTHTYPE,=C'AH '                                                  
         CLI   TSHTYPE,C'A'                                                     
         BE    TRRPT20                                                          
*                                                                               
         MVC   PTHTYPE,=C'MAR'                                                  
         CLI   TSHTYPE,C'M'                                                     
         BE    TRRPT20                                                          
*                                                                               
         MVC   PTHTYPE,=C'INV'                                                  
         CLI   TSHTYPE,C'I'                                                     
         BE    TRRPT20                                                          
*                                                                               
         MVC   PTHTYPE,=C'BIL'                                                  
         CLI   TSHTYPE,C'B'                                                     
         BE    TRRPT20                                                          
         MVC   PTHTYPE,=C'???'                                                  
*                                                                               
TRRPT20  OC    TSHDATE,TSHDATE     IS THERE A HELD DATE?                        
         BZ    TRRPT30             NO                                           
         GOTO1 DATCON,DMCB,(1,TSHDATE),(5,PTHDATE)                              
*                                                                               
TRRPT30  LA    R6,TRTBUCKS                                                      
         BAS   RE,ZAPBUCKS                                                      
*                                                                               
         ZAP   TRTAMT,TSKAMT                                                    
         LA    R7,TRTBABLE                                                      
         TM    TSSTAT,TSHELD                                                    
         BZ    *+8                                                              
         LA    R7,TRTHELD                                                       
*                                                                               
         AP    0(6,R7),TSKAMT                                                   
*                                                                               
         LA    R6,TRTBUCKS                                                      
         BAS   RE,PRTBUCKS                                                      
*                                                                               
         BAS   RE,PRINTEM                                                       
         BAS   RE,BUMPEM                                                        
         MVC   PREVTRN,TRNKEY                                                   
         AP    CACNT,=P'1'                                                      
*                                                                               
         LA    R2,TSORTLN(R2)                                                   
         BCT   R0,TRRPT10                                                       
*                                                                               
         MVI   PREVTRN,C' '                                                     
         BAS   RE,TRLAST           DO LAST LASTS                                
*                                                                               
         MVC   PTWC(15),=C'** JOB TOTAL **'                                     
         LA    R6,ACCBUCKS                                                      
         BAS   RE,PRTBUCKS                                                      
         BAS   RE,PRINTEM                                                       
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------           
*        PRINT WC/WCNAME AND CA/CA NAME                                         
*--------------------------------------------------------------------           
TRFIRST  NTR1                                                                   
         USING TRNRECD,R3                                                       
         USING PTRND,R4                                                         
         LA    R4,P                                                             
         CLC   TRNKEY((TRNKWORK-TRNKEY)+L'TRNKWORK),PREVTRN                     
         BE    TRF50                                                            
*                                                                               
         MVC   P,SPACES                                                         
         MVC   PTWC,TRNKWORK                                                    
         LA    R1,TRNKWORK                                                      
         LA    R5,PTWC+3                                                        
         BAS   RE,WCNAME                                                        
         LA    R5,PTWC+L'P                                                      
         GOTO1 =V(UNDERLIN),DMCB,(18,PTWC),(R5)                                 
         BAS   RE,PRINTEM                                                       
         ZAP   WCCNT,=P'0'                                                      
         LA    R6,WCBUCKS                                                       
         BAS   RE,ZAPBUCKS                                                      
*                                                                               
TRF50    CLC   TRNKEY((TRNKCACT-TRNKEY)+L'TRNKCACT),PREVTRN                     
         BE    TRFX                                                             
         MVC   PTVEND(14),TRNKCUNT U/L/CONTRA ACCOUNT                           
*                                                                               
         USING LISTD,R2            GET CAC NAME                                 
         LA    R2,LISTDATA                                                      
         MVC   LISTKEY,TRNKCUNT                                                 
         L     R2,ACACLST                                                       
         BAS   RE,GETACNM                                                       
         LA    R2,LISTDATA                                                      
         LA    R6,3                NUMBER OF LINES TO CHOP INTO                 
         GOTO1 CHOPPER,DMCB,(36,LISTNAME),('LPTVNAME',PTVNAME),        X        
               (C'P',(R6))                                                      
         ZAP   CACNT,=P'0'                                                      
         AP    WCCNT,=P'1'                                                      
         LA    R6,CACBUCKS                                                      
         BAS   RE,ZAPBUCKS                                                      
TRFX     B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------           
*        PRINT LAST CAC AND LAST WC IF NEEDED                                   
*--------------------------------------------------------------------           
TRLAST   NTR1                                                                   
*                                                                               
         OC    PREVTRN,PREVTRN     IS THERE A PREVIOUS TRAN                     
         BZ    TRLX                NO                                           
*                                                                               
         USING TRNRECD,R3                                                       
         USING PTRND,R4                                                         
         LA    R4,P                                                             
         CLC   TRNKEY((TRNKCACT-TRNKEY)+L'TRNKCACT),PREVTRN                     
         BE    TRL50                                                            
         CP    CACNT,=P'1'        MORE THAN 1 CA IN THESE TOTALS                
         BNH   TRL50               NO                                           
         MVC   PTVEND(10),=C'TOTALS FOR'                                        
         MVC   PTVEND+11(14),PREVTRN+18                                         
         LA    R6,CACBUCKS                                                      
         BAS   RE,PRTBUCKS                                                      
         MVI   SPACING,2                                                        
         BAS   RE,PRINTEM                                                       
*                                                                               
TRL50    CLC   TRNKEY((TRNKWORK-TRNKEY)+L'TRNKWORK),PREVTRN                     
         BE    TRLX                                                             
*                                                                               
         CP    WCCNT,=P'1'        MORE THAN 1 CA IN THESE TOTALS                
         BNH   TRLX                NO                                           
         MVC   PTWC(10),=C'TOTALS FOR'                                          
         MVC   PTWC+11(2),PREVTRN+15                                            
         LA    R6,WCBUCKS                                                       
         BAS   RE,PRTBUCKS                                                      
         MVI   SPACING,2                                                        
         BAS   RE,PRINTEM                                                       
TRLX     B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------           
*        PRINT THE BUCKETS AT 0(R6)                                             
*--------------------------------------------------------------------           
PRTBUCKS NTR1                                                                   
         USING PTRND,R4                                                         
         LA    R4,P                                                             
         LA    R5,PTAMNT                                                        
         BAS   RE,EDTAMT                                                        
         LA    R6,6(R6)                                                         
         LA    R5,PTHELD1                                                       
         BAS   RE,EDTAMT                                                        
         LA    R6,6(R6)                                                         
         LA    R5,PTBABLE                                                       
         BAS   RE,EDTAMT                                                        
         B     XIT                                                              
*                                                                               
EDTAMT   EQU   *                                                                
         CP    0(6,R6),=P'0'                                                    
         BER   RE                                                               
*                                                                               
         EDIT  (P6,0(R6)),(13,0(R5)),2,MINUS=YES                                
         BR    RE                                                               
         EJECT                                                                  
*--------------------------------------------------------------------           
*        ZAP THE BUCKETS AT 0(R6)                                               
*--------------------------------------------------------------------           
ZAPBUCKS NTR1                                                                   
         LA    R1,NBUCKS                                                        
ZAPB10   ZAP   0(6,R6),=P'0'                                                    
         LA    R6,6(R6)                                                         
         BCT   R1,ZAPB10                                                        
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------           
*        ADD THE TRANSACTION LEVEL BUCKETS TO THE HIGHER LEVELS                 
*--------------------------------------------------------------------           
BUMPEM   NTR1                                                                   
         LA    R0,NLEVELS                                                       
         LA    R3,CACBUCKS                                                      
BUMP10   LA    R1,NBUCKS                                                        
         LA    R2,TRTBUCKS                                                      
BUMP20   AP    0(6,R3),0(6,R2)                                                  
         LA    R3,6(R3)                                                         
         LA    R2,6(R2)                                                         
         BCT   R1,BUMP20                                                        
         BCT   R0,BUMP10                                                        
         B     XIT                                                              
         EJECT                                                                  
PRINTEM  NTR1                                                                   
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*--------------------------------------------------------------------           
*        START BOXES FOR TRANSACTION DETAIL                                     
*--------------------------------------------------------------------           
BXTOP    NTR1                                                                   
         MVC   P,SPACES                                                         
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLS,SPACES                                                   
         ZIC   RF,LINE                                                          
         LA    RE,BOXROWS(RF)                                                   
         BCTR  RE,0                                                             
*                                                                               
         MVI   0(RE),C'T'                                                       
         MVI   3(RE),C'M'                                                       
         MVI   BOXROWS+56,C'B'                                                  
*                                                                               
         USING PTRND,R5                                                         
         LA    R5,BOXCOLS                                                       
         MVI   PTLEFT,C'L'                                                      
         MVI   PTCOL1,C'C'                                                      
         MVI   PTCOL2,C'C'                                                      
         MVI   PTCOL3,C'C'                                                      
         MVI   PTCOL4,C'C'                                                      
         MVI   PTCOL5,C'C'                                                      
         MVI   PTCOL6,C'C'                                                      
         MVI   PTRIGHT,C'R'                                                     
         MVI   BOXINIT,0                                                        
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*        SET UP MIDLINES WITH DATA FROM TRANSACTION DETAILS                     
*---------------------------------------------------------------------          
SETMID   NTR1                                                                   
         L     RF,=A(MYMID1)                                                    
         MVC   MID1+TRREPOFF(L'MID1-TRREPOFF),0(RF)                             
         L     RF,=A(MYMID2)                                                    
         MVC   MID2+TRREPOFF(L'MID1-TRREPOFF),0(RF)                             
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        STORE ACCOUNT NAMES                                                    
*        A(20 EL) IS IN R4, SAVE IN NAMENEXT                                    
*        ACCOUNT CODE IS IN LISTKEY OFF LISTDATA                                
*        A(BINSRCH TABLE FOR THIS KEY) IS IN R3 ON ENTRY                        
*        PUT KEY, NAMENEXT TO BINSRCH AS NMPTKEY, NMPTPTR                       
*        BUMP NAMENEXT                                                          
*----------------------------------------------------------------------         
         USING LISTD,R2                                                         
         USING NMPTD,R5                                                         
PUTACNM  NTR1                                                                   
         LA    R2,LISTDATA                                                      
         LA    R5,NMPTDATA                                                      
         MVC   NMPTKEY,LISTKEY                                                  
         SPACE 1                                                                
         ZIC   R1,1(R4)                                                         
         L     RF,NAMENEXT         MAKE SURE I HAVE ENOUGH ROOM                 
         L     RE,NAMEEND                                                       
         AR    RF,R1                                                            
         CR    RF,RE                                                            
         BNH   *+6                                                              
         DC    H'0'                NAME TABLE IS FULL                           
         SPACE 1                                                                
         BCTR  R1,0                                                             
         L     RE,NAMENEXT                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R4)       SAVE 20 EL                                   
*                                                                               
         STCM  RE,15,NMPTPTR       SAVE A(WHERE I PUT THIS 20 EL)               
         ST    RF,NAMENEXT         SAVE A(WHERE THE NEXT ONE GOES)              
*                                                                               
         GOTO1 BINADD,DMCB,(R5),(R3)                                            
         B     XIT                 BINADD SETS CC TO EQ IF ADDED                
         DROP  R2,R5                                                            
         EJECT                                                                  
*----------------------------------------------------------------------         
*        GET ACCOUNT NAMES KEY IS FIRST 12 OF LISTDATA                          
*        KEY IS IN LIST LEY OF LIST DATA                                        
*        TABLE IS AT 0(R2)                                                      
*        RETURN THE NAME IN LISTNAME                                            
*----------------------------------------------------------------------         
         USING BIND,R2                                                          
         USING LISTD,R4                                                         
GETACNM  NTR1                                                                   
         LA    R4,LISTDATA                                                      
         MVC   LISTNAME,SPACES                                                  
         SPACE 1                                                                
         LA    R6,BINTABLE                                                      
         MVC   DMCB+8(16),BININ                                                 
         GOTO1 BINSRCH,DMCB,(X'00',(R4)),(R6)                                   
         CLI   DMCB,0                                                           
         BE    *+6               FOUND                                          
         DC    H'0'                                                             
*                                                                               
         USING NMPTD,R2                                                         
         L     R2,DMCB                                                          
         LA    R5,LISTNAME                                                      
         ICM   R4,15,NMPTPTR                                                    
         BAS   RE,GETNAME                                                       
         B     XIT                                                              
         DROP  R2,R4                                                            
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        ADD AN ITEM TO THE BINSRCH TABLE                                       
*        P1 IS ITEM, P2 IS A(TABLE)                                             
*----------------------------------------------------------------------         
         USING BIND,R5                                                          
BINADD   NTR1                                                                   
         L     R5,4(R1)                                                         
         MVC   DMCB+8(16),BININ    NUMBER, LEN, KEY, MAX                        
         LA    R6,BINTABLE                                                      
         L     R4,0(R1)                                                         
         GOTO1 BINSRCH,DMCB,(X'01',(R4)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   BINAOK                                                           
         SPACE 1                                                                
         CR    RB,R1               TABLE IS FULL, SET NEQ CC                    
         B     BINAX                                                            
         SPACE 1                                                                
BINAOK   MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CR    RB,RB               SET EQ CC                                    
BINAX    B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        GET AND PRINT A WORKCODE NAME                                          
*        0(R1) IS THE WORK CODE TO PRINT                                        
*        0(R5) IS WHERE TO PRINT THE NAME                                       
*----------------------------------------------------------------------         
         USING WCOELD,R4                                                        
WCNAME   NTR1                                                                   
         MVC   0(L'WCODESC,R5),SPACES                                           
         L     R4,ADLEDGER                                                      
         MVI   ELCODE,WCOELQ                                                    
         BAS   RE,GETEL                                                         
         BE    WCN10                                                            
         DC    H'0'                                                             
*                                                                               
WCN10    CLC   WCOCODE,0(R1)                                                    
         BE    WCN50                                                            
         BAS   RE,NEXTEL                                                        
         BE    WCN10                                                            
         DC    H'0'                WORK CODE NOT FOUND                          
*                                                                               
WCN50    MVC   0(L'WCODESC,R5),WCODESC                                          
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        R4 IS ADDRESS OF THE 20 ELEMENT                                        
*        R5 IS ADDRESS OF 36 BYTE AREA                                          
*----------------------------------------------------------------------         
         USING NAMELD,R4                                                        
GETNAME  MVC   0(36,R5),SPACES                                                  
         ZIC   R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         EX    R1,GETNMVC                                                       
         BR    RE                                                               
GETNMVC  MVC   0(0,R5),NAMEREC                                                  
         DROP  R4                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        SAVE THE READ SEQUENCE                                                 
*----------------------------------------------------------------------         
***********************************************************************         
*AVESEQ  NTR1                                                         *         
*        L     RE,ADACCFIL         A(ACC DIR)                         *         
*        L     RE,ISPDKEY-ISDTF(RE)                                   *         
*        MVC   DCBKEY,0(RE)                                           *         
*        B     XIT                                                    *         
*                                                                     *         
*                                                                     *         
*----------------------------------------------------------------------         
*        RESTORE THE READ SEQUENCE                                    *         
*----------------------------------------------------------------------         
*ESTSEQ  NTR1                                                         *         
*        LA    R3,DCBKEY                                              *         
*        BAS   RE,READ                                                *         
*        B     XIT                                                    *         
*                                                                     *         
***********************************************************************         
*                                                                               
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        READ THE RECORD WHOSE KEY IS AT 0(R3) FOR UPDATE                       
*----------------------------------------------------------------------         
READUP   NTR1                                                                   
         LA    R2,X'80'            READ FOR UPDATE                              
         L     R5,AMYIO                                                         
         MVC   MYKEY,0(R3)                                                      
         MVC   COMMAND,=CL8'DMREAD'                                             
         B     CALLDMG                                                          
*                                                                               
*----------------------------------------------------------------------         
*        READ THE RECORD WHOSE KEY IS AT 0(R3)                                  
*----------------------------------------------------------------------         
READ     NTR1                                                                   
         SR    R2,R2                                                            
         L     R5,AMYIO                                                         
         MVC   MYKEY,0(R3)                                                      
         MVC   COMMAND,=CL8'DMREAD'                                             
         B     CALLDMG                                                          
*                                                                               
*----------------------------------------------------------------------         
*        WRITE THE RECORD IN AMYIO BACK TO THE FILE                             
*----------------------------------------------------------------------         
WRITE    NTR1                                                                   
         SR    R2,R2                                                            
         CLI   QOPT2,C'Y'                                                       
         BNE   XIT                                                              
         L     R5,AMYIO                                                         
         MVC   MYKEY,0(R5)                                                      
         MVC   COMMAND,=CL8'DMWRT'                                              
*                                                                               
CALLDMG  GOTO1 DATAMGR,DMCB,((R2),COMMAND),=C'ACCOUNT',MYKEY,(R5)               
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        PUT A TRANSACTION RECORD TO QSORT AREA                                 
*----------------------------------------------------------------------         
PUTSORT  NTR1                                                                   
         USING TRNELD,R4                                                        
         L     R4,ADTRANS                                                       
         USING TSORTD,R2                                                        
         XC    SORTREC,SORTREC                                                  
         LA    R2,SORTREC                                                       
         ZAP   TSKAMT,TRNAMNT      AMOUNT TO KEY                                
*                                                                               
         MVI   TSSIGN,POSITIVE                                                  
         CP    TRNAMNT,=P'0'                                                    
         BH    *+8                                                              
         MVI   TSSIGN,NEGATIVE                                                  
*                                                                               
         MVC   TSSEQ,TRNCNT        SEQUENCE NUMBER OF THIS TRAN IN              
*                                                                               
         SH    R4,DATADISP         SAVE TRANSACTION KEY                         
         MVC   TSTRNKEY(L'TRNKEY),0(R4)                                         
*                                                                               
         L     R4,ADTRANS                                                       
         TM    TRNSTAT,TRNSHOLD    IS THIS TRAN HELD                            
         BNO   PUTS50              NO                                           
*                                                                               
         OI    TSSTAT,TSHELD       FLAG AS PREVIOUSLY HELD FOR REPORT           
         MVI   TSHTYPE,C'I'        ASSUME HELD BY =INV                          
*                                                                               
         USING GDAELD,R4           GENERAL DATE EL                              
         MVI   ELCODE,GDAELQ       GET POSSIBLE HELD DATE                       
         BAS   RE,NEXTEL                                                        
         BNE   PUTS50                                                           
         CLI   GDATYPE,GDATAHLD    IS THIS A DATE AUTOHELD                      
         BNE   PUTS10              NO                                           
         MVI   TSHTYPE,C'A'                                                     
         B     PUTS45                                                           
*                                                                               
PUTS10   CLI   GDATYPE,GDATMHLD    MARKER HELD?                                 
         BNE   PUTS20              NO, SEE IF BILLING                           
         MVI   TSHTYPE,C'M'                                                     
         B     PUTS45                                                           
*                                                                               
PUTS20   CLI   GDATYPE,GDATBHLD    HELD BY BILLING?                             
         BNE   PUTS50              NO, DON'T KNOW WHAT IT IS                    
         MVI   TSHTYPE,C'B'                                                     
*                                                                               
PUTS45   MVC   TSHDATE,GDADATE     SAVE DATE HELD                               
*                                                                               
PUTS50   L     R3,ASORT            AREA TO BUILD SORT TABLE                     
         LH    R5,TRNCNT           NUMBER OF RECORDS IN TABLE                   
*                                                                               
         SR    R4,R4                                                            
         SR    R6,R6                                                            
         LA    R6,TSORTLN                                                       
         MR    R4,R6                                                            
         AR    R3,R5               ADDRESS OF NEXT AVAILABLE SPACE              
*                                                                               
         MVC   0(TSORTLN,R3),SORTREC                                            
*                                                                               
         LH    R1,TRNCNT           BUMP TRNCNT                                  
         LA    R1,1(R1)                                                         
         STH   R1,TRNCNT                                                        
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        SORT THE RECORDS IN SORT AREA                                          
*        BY TRANSACTION AMOUNT                                                  
*----------------------------------------------------------------------         
SORTAMT  NTR1                                                                   
         L     R3,ASORT            AREA TO BUILD SORT TABLE                     
         LH    R4,TRNCNT           NUMBER OF RECORDS IN TABLE                   
         LA    R5,TSORTLN          LENGTH OF RECORD                             
         LA    R6,TSKEYLN          LENGTH OF KEY                                
*                                                                               
         GOTO1 AQSORT,DMCB,('D',(R3)),(R4),(R5),(R6),0                          
*                                                                               
         B     XIT                                                              
D        EQU   C'D'                                                             
         EJECT                                                                  
*----------------------------------------------------------------------         
*        SORT THE RECORDS IN SORT AREA                                          
*        BY SEQUENCE NUMBER                                                     
*----------------------------------------------------------------------         
SORTSEQ  NTR1                                                                   
         L     R3,ASORT            AREA TO BUILD SORT TABLE                     
         LH    R4,TRNCNT           NUMBER OF RECORDS IN TABLE                   
         LA    R5,TSORTLN          LENGTH OF RECORD                             
         LA    R6,2                L'KEY                                        
         LA    R7,TSSEQ-TSORTD     DISPLACEMENT OF KEY INTO RECORD              
*                                                                               
         GOTO1 AQSORT,DMCB,(0,(R3)),(R4),(R5),(R6),(R7)                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        SET UP JOB LEVEL STUFF (DATES, STATUS, ETC)                            
*----------------------------------------------------------------------         
JOBDATA  NTR1                                                                   
         MVI   JOBINFO,C' '                                                     
         MVC   JOBINFO+1(JBLEN-1),JOBINFO                                       
*                                                                               
         L     R6,ADGOBLOC                                                      
         USING GOBLOCKD,R6                                                      
         CLI   GOEFFOFC,0                                                       
         BE    *+10                                                             
         MVC   JBOFFICE,GOEFFOFC                                                
         CLI   GOBILTYP,0                                                       
         BE    JOBD60                                                           
         MVC   JBBLTYPE(5),=C'TOTAL'                                            
         CLI   GOBILTYP,C'T'                                                    
         BE    JOBD60                                                           
         MVC   JBBLTYPE(8),=C'ONE-LINE'                                         
         CLI   GOBILTYP,C'1'                                                    
         BE    JOBD60                                                           
         MVC   JBBLTYPE(10),=C'UNBILLABLE'                                      
         CLI   GOBILTYP,C'U'                                                    
         BE    JOBD60                                                           
         MVC   JBBLTYPE(11),=C'PROGRESSIVE'                                     
         CLI   GOBILTYP,C'P'                                                    
         BE    JOBD60                                                           
         MVC   JBBLTYPE(11),=CL11'CLIENT'                                       
         CLI   GOBILTYP,C'C'                                                    
         BE    JOBD60                                                           
*                                                                               
         MVC   JBBLTYPE,SPACES     START OVER AGAIN                             
         EDIT  (4,GOBILAM1),(12,JBBLTYPE),2,ALIGN=LEFT                          
         LA    R4,JBBLTYPE+1                                                    
JOBD50   CLI   0(R4),C' '          GET PAST AMOUNT                              
         BE    JOBD55                                                           
         LA    R4,1(R4)                                                         
         B     JOBD50                                                           
*                                                                               
JOBD55   MVC   1(3,R4),=C'FEE'                                                  
         CLI   GOBILTYP,C'F'                                                    
         BE    JOBD60                                                           
*                                                                               
         MVC   1(14,R4),=C'SPECIAL AMOUNT'                                      
         CLI   GOBILTYP,C'S'                                                    
         BE    JOBD60                                                           
*                                                                               
         SH    R4,=H'3'            TO CHOP OFF DECIMALS                         
         MVC   0(20,R4),=CL20' PC OF EST'                                       
*                                                                               
JOBD60   OC    LAST,LAST                                                        
         BZ    JOBD70                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(1,LAST),(5,JBLASTBL)                                
*                                                                               
JOBD70   L     R4,ADACC                                                         
         MVI   ELCODE,X'26'        JOB DATES                                    
         BAS   RE,GETEL                                                         
         BNE   JOBD80                                                           
         USING JOBELD,R4                                                        
         GOTO1 DATCON,DMCB,(1,JOBADATE),(5,JBOPEN)                              
         GOTO1 DATCON,DMCB,(1,JOBCDATE),(5,JBCLOSED)                            
         USING RSTELD,R4                                                        
         L     R4,ADACCSTA                                                      
         TM    RSTSTAT,RSTSACIC    ACCOUNT IS CLOSED                            
         BZ    JOBD80                                                           
         MVC   JBCLOSED+9(3),=C'(C)'     THEN MARK DATE                         
         DROP  R4                                                               
*                                                                               
JOBD80   L     R4,ADACCSTA                                                      
         USING RSTELD,R4                                                        
*                                                                               
         CLI   RSTFILT1,C' '                                                    
         BNH   *+10                                                             
         MVC   JBF1,RSTFILT1                                                    
*                                                                               
         CLI   RSTFILT2,C' '                                                    
         BNH   *+10                                                             
         MVC   JBF2,RSTFILT2                                                    
*                                                                               
         CLI   RSTFILT3,C' '                                                    
         BNH   *+10                                                             
         MVC   JBF3,RSTFILT3                                                    
*                                                                               
         CLI   RSTFILT4,C' '                                                    
         BNH   *+10                                                             
         MVC   JBF4,RSTFILT4                                                    
*                                                                               
         CLI   RSTFILT5,C' '                                                    
         BNH   *+10                                                             
         MVC   JBF5,RSTFILT5                                                    
*                                                                               
         XC    JBPOB,JBPOB         ENSURE THIS WILL TAKE UP A HEAD LINE         
         L     R4,ADACC                                                         
         USING PPRELD,R4                                                        
         MVI   ELCODE,X'24'        GET POB                                      
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   JBPOB,PPRBILLP                                                   
*                                                                               
JOBDX    XIT1                                                                   
*----------------------------------------------------------------------         
*        GET SPACE FOR THE QSORT TABLE                                          
*        AND CONTRA ACCOUNT NAME TABLES                                         
*----------------------------------------------------------------------         
GETBUFF  NTR1                                                                   
         L     R0,=A(BUFSIZE)     SUM OF ALL TABLE SIZES                        
         GETMAIN R,LV=(0)                                                       
         LA    R0,MAINNUM                                                       
         LR    R5,R1               R5 IS BUFFER POINTER                         
         ST    R1,ABUFF            SAVE BUFF START                              
*                                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD          PRINT THE BUFFER IN DUMP                     
         USING MASTD,R2                                                         
         STCM  R5,15,MCUSRDMP                                                   
         LR    RF,R5                                                            
         A     RF,=A(BUFSIZE)                                                   
         STCM  RF,15,MCUSRDMP+4                                                 
*                                                                               
         L     R2,AMAINTAB                                                      
         USING MAIND,R2                                                         
*                                                                               
GETB10   MVC   *+8(2),MAINAST     SCON OF WHERE TO STORE BUFF LOCATION          
         ST    R5,FULL             FULL IS A DUMMY FOR THE ASSEMBLER            
*                                                                               
         CLI   MAINTYPE,MAINBIN    IS THIS A BINSRCH TABLE                      
         BNE   GETB30              NO                                           
*                                                                               
         USING BIND,R5                                                          
         XC    BININ,BININ         SET UP BINSRCH HEADER                        
         MVC   BINLEN,=A(NMPTDLN)                                               
         MVC   BINDISPK,=A(L'NMPTKEY)                                           
         MVC   BINMAX,MAINMAX                                                   
*                                                                               
GETB30   A     R5,MAINSIZE                                                      
*                                                                               
         LA    R2,MAINLEN(R2)                                                   
         BCT   R0,GETB10                                                        
*                                                                               
         L     R5,ANAMES           GET ADDRESSES FOR PROCESSING NAMES           
         ST    R5,NAMENEXT         SAVE START AS A(NEXT AVAILABLE)              
         A     R5,=A(NAMESIZE)     CALC ENDING ADDRESS                          
         ST    R5,NAMEEND                                                       
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        RELEASE GETMAINED SPACE                                                
*----------------------------------------------------------------------         
RELBUFF  NTR1                                                                   
         L     R1,ABUFF                                                         
         L     R0,=A(BUFSIZE)     SUM OF ALL TABLE SIZES                        
         FREEMAIN R,A=(1),LV=(0)                                                
*                                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD          PRINT THE BUFFER IN DUMP                     
         USING MASTD,R2                                                         
         XC    MCUSRDMP,MCUSRDMP   CLEAR XTRA DUMP ADDRESS                      
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE TO DELETE AN ELEMENT                                     
*                                                                               
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
*---------------------------------------------------------------------          
*                                                                               
DELEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'D',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))           
         B     XIT                                                              
*                                                                               
*---------------------------------------------------------------------          
*                                                                               
*              ROUTINE TO ADD AN ELEMENT                                        
*                                                                               
*              P1   A(RECORD)                                                   
*              P2   A(ELEMENT)                                                  
*---------------------------------------------------------------------          
*                                                                               
ADDEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         GOTO1 HELLO,DMCB,(C'P',=C'ACCOUNT '),(R2),(R3)                         
         CLI   DMCB+12,0                                                        
         BE    XIT                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
HELLO    DC    V(HELLO)                                                         
AQSORT   DC    V(QSORT)                                                         
AMAINTAB DC    A(MAINTAB)                                                       
         DC    X'FF'                                                            
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
*        SCONS TO PRINT ACCOUNT DATA                                            
*        NOTE: THE ORDER OF THESE SCONS MUST MIRROR THE ORDER OF                
*        ACCACCS                                                                
*----------------------------------------------------------------------         
*                                                                               
         USING PACCD,R5                                                         
PACCSCON DS    0H                                                               
         DC    S(PAHIREV)                                                       
         DC    S(PACUREST)                                                      
         DC    S(PAOVAMT)                                                       
         DC    S(PAMAXEST)                                                      
         DC    S(PAACTUAL)                                                      
         DC    S(PABILLED)                                                      
         DC    S(PAHOLDTB)                                                      
         DC    S(PAHELD)                                                        
         DC    S(PANEEDTB)                                                      
         DC    S(PABLABLE)                                                      
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        MAINTAB IS TABLE OF HOW GETMAIN CORE SHOULD BE SPLIT UP                
*--------------------------------------------------------------------*          
MAINTAB  DS    0F                                                               
         DC    S(ACACLST)                                                       
         DC    AL1(MAINBIN)        SET BINSRCH HEADERS FOR THIS                 
         DS    AL1                                                              
         DC    A(CACMAX)                                                        
         DC    A(CACSIZE)                                                       
*                                                                               
         DC    S(ASORT)                                                         
         DC    AL1(0)                                                           
         DS    AL1                                                              
         DC    A(SORTMAX)                                                       
         DC    A(SORTSIZE)                                                      
*                                                                               
         DC    S(AMYIO)                                                         
         DC    AL1(0)                                                           
         DS    AL1                                                              
         DC    A(0)                                                             
         DC    A(MYIOSIZE)                                                      
*                                                                               
         DC    S(ANAMES)                                                        
         DC    AL1(0)                                                           
         DS    AL1                                                              
         DC    A(0)                                                             
         DC    A(NAMESIZE)                                                      
*                                                                               
MAINNUM  EQU   (*-MAINTAB)/MAINLEN                                              
*                                                                               
CACMAX   EQU   2000                                                             
CACSIZE  EQU   BINLENQ+(CACMAX*NMPTDLN)                                         
*                                                                               
NAMESIZE EQU   40000               TABLE OF CONTRA ACCOUNT NAMES                
*                                                                               
MYIOSIZE EQU   2000                SPACE TO READ TRANSACTIONS                   
*                                                                               
SORTMAX  EQU   5000                MAX 5000 TRANSACTIONS/JOB                    
SORTSIZE EQU   SORTMAX*TSORTLN                                                  
*                                                                               
BUFSIZE  EQU   SORTSIZE+CACSIZE+NAMESIZE                                        
         EJECT                                                                  
MYMID1   DC    CL132'  WORK CODE                   DATE       INVOICE  X        
                   HELD   TYPE     HELD         TO BE '                         
MYMID2   DC    CL132'  VENDOR                      NUMBER     AMOUNT   X        
                   DATE           AMOUNT        BILLED'                         
         EJECT                                                                  
HDHOOK   NMOD1 0,*HDKH*                                                         
         L     RC,SAVERC                                                        
*                                                                               
         MVC   HEAD4+9(6),CLICODE                                               
         MVC   HEAD4+19(36),CLINAME                                             
*                                                                               
         MVC   HEAD5+9(6),PROCODE                                               
         MVC   HEAD5+19(36),PRONAME                                             
*                                                                               
         MVC   HEAD6+9(6),JOBCODE                                               
         MVC   HEAD6+19(36),JOBNAME                                             
*                                                                               
         MVC   HEAD1+51(30),TITLE                                               
         MVC   HEAD2+51(30),TITLE2                                              
         MVC   HEAD5+61(9),RUNTYPE                                              
*                                                                               
         MVC   HEAD3+109(8),JBOPEN                                              
         MVC   HEAD4+109(12),JBCLOSED                                           
         MVC   HEAD5+109(8),JBLASTBL                                            
         MVC   HEAD6+109(25),JBBLTYPE                                           
         MVC   HEAD7+19(50),JBPOB                                               
*                                                                               
         MVC   HEAD7+97(29),=C'F1= ,F2= ,F3= ,F4= ,F5= ,OFF='                   
         MVC   HEAD7+100(1),JBF1                                                
         MVC   HEAD7+105(1),JBF2                                                
         MVC   HEAD7+110(1),JBF3                                                
         MVC   HEAD7+115(1),JBF4                                                
         MVC   HEAD7+120(1),JBF5                                                
         MVC   HEAD7+126(2),JBOFFICE                                            
         B     HDH20                                                            
*                                                                               
HDH20    CLI   RCSUBPRG,0                                                       
         BNE   HDH40                                                            
*                                                                               
         OC    HIREV#,HIREV#                                                    
         BZ    HDH30                                                            
         MVI   HEAD10+11,C'R'                                                   
         LA    R3,HEAD10+12                                                     
         ZIC   R1,HIREV#                                                        
         BAS   RE,PRTNUM                                                        
*                                                                               
HDH30    OC    CUREST#,CUREST#                                                  
         BZ    HDH40                                                            
         MVI   HEAD10+25,C'R'                                                   
         LA    R3,HEAD10+26                                                     
         ZIC   R1,CUREST#                                                       
         BAS   RE,PRTNUM                                                        
*                                                                               
HDH40    MVC   HEAD9+118(4),=C'HOLD'                                            
         MVC   HEAD10+117(6),=C'TARGET'                                         
         CLI   QOPT1,C' '                                                       
         BE    HDH50                                                            
         MVC   HEAD9+117(5),=C'TO BE'                                           
         MVC   HEAD10+117(6),=C'UNHELD'                                         
*                                                                               
HDH50    L     R7,ADBOX                                                         
*                                                                               
         USING BOXD,R7                                                          
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLS,SPACES                                                   
*                                                                               
         CLI   RCSUBPRG,0                                                       
         BNE   BX40                                                             
*                                                                               
         MVI   BOXROWS+7,C'T'                                                   
         MVI   BOXROWS+10,C'M'                                                  
         MVI   BOXROWS+13,C'B'                                                  
*                                                                               
         LA    R2,BOXCOLS+1                                                     
         MVI   0(R2),C'L'                                                       
*                                                                               
         LA    R0,BXNMCOLS                                                      
BX30     LA    R2,BXWIDTH(R2)                                                   
         MVI   0(R2),C'C'                                                       
         BCT   R0,BX30                                                          
*                                                                               
         MVI   0(R2),C'R'                                                       
         B     BX50                                                             
*                                                                               
         USING PTRND,R5                                                         
BX40     MVI   BOXROWS+7,C'T'                                                   
         MVI   BOXROWS+10,C'M'                                                  
*                                                                               
         L     RF,=A(MYMID1)                                                    
         MVC   HEAD9+TRREPOFF(L'HEAD1-TRREPOFF),0(RF)                           
         L     RF,=A(MYMID2)                                                    
         MVC   HEAD10+TRREPOFF(L'HEAD1-TRREPOFF),0(RF)                          
*                                                                               
         LA    R5,BOXCOLS          TRANSACTION DETAIL BOXES                     
         MVI   PTLEFT,C'L'                                                      
         MVI   PTCOL1,C'C'                                                      
         MVI   PTCOL2,C'C'                                                      
         MVI   PTCOL3,C'C'                                                      
         MVI   PTCOL4,C'C'                                                      
         MVI   PTCOL5,C'C'                                                      
         MVI   PTCOL6,C'C'                                                      
         MVI   PTRIGHT,C'R'                                                     
*                                                                               
BX50     MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         XMOD1                                                                  
         DROP  R7                                                               
*                                                                               
PRTNUM   ST    RE,SAVERE                                                        
         EDIT  (R1),(3,(R3)),ALIGN=LEFT                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         LTORG                                                                  
*                                                                               
SAVERC   DS    A                                                                
*                                                                               
BXNMCOLS EQU   9                                                                
BXWIDTH  EQU   14                                                               
         EJECT                                                                  
ACAHD    DSECT                                                                  
ADBOX    DS    A                                                                
ABUFF    DS    A                                                                
ACACLST  DS    A                   LIST OF CONTRA ACCOUNT KEYS                  
ASORT    DS    A                   ADDESS OF SORTER TABLE                       
ANAMES   DS    A                   TABLE OF 20 ELEMENTS                         
NAMENEXT DS    A                   ADDRESS OF NEXT AVAILIBLE AREA               
NAMEEND  DS    A                   END OF NAME TABLE                            
AMYIO    DS    A                   IO AREA                                      
SAVERE   DS    A                                                                
TRNCNT   DS    H                                                                
TODAY3   DS    CL3                                                              
LAST     DS    CL3                 HIGHEST DATE ON ACCOUNTS BILLS               
SVREF    DS    CL6                                                              
SVDATE   DS    CL3                                                              
RUNCNT   DS    PL6                                                              
ACTIVITY DS    CL1                                                              
SORTREC  DS    CL(TSORTLN)                                                      
*                                  ACCOUNT ACCUMULATORS                         
ACCACCS  DS    0C                  ACCOUNT REPORT ACCS                          
HIREV    DS    PL6                                                              
CUREST   DS    PL6                                                              
MAXAMT   DS    PL6                                                              
MAXEST   DS    PL6                                                              
ACTUAL   DS    PL6                                                              
BILLED   DS    PL6                                                              
HOLDTOBL DS    PL6                 AMOUNT I NEED TO HOLD TO BILL                
HELD     DS    PL6                 PREVIOUSLY HELD                              
TOBEHELD DS    PL6                 AMOUNT I NEED TO HOLD NOW                    
BILLABLE DS    PL6                 BILLABLE, ON SECOND LINE                     
ACCACCNM EQU   (*-ACCACCS)/6                                                    
*                                                                               
BUCKS    DS    0C                                                               
TRTBUCKS DS    0C                                                               
TRTAMT   DS    PL6                 TRANSACTION TOTALS                           
TRTHELD  DS    PL6                                                              
TRTBABLE DS    PL6                                                              
NBUCKS   EQU   (*-BUCKS)/6         NUMBER OF BUCKETS PER LEVEL                  
*                                                                               
CACBUCKS DS    0C                                                               
CACAMT   DS    PL6                 CONTRA ACCOUNT TOTALS                        
CACHELD  DS    PL6                                                              
CACBABLE DS    PL6                                                              
*                                                                               
WCBUCKS  DS    0C                  WORK CODE BUCKETS                            
WCAMT    DS    PL6                                                              
WCHELD   DS    PL6                                                              
WCBABLE  DS    PL6                                                              
*                                                                               
ACCBUCKS DS    0C                                                               
ACCAMT   DS    PL6                 JOB BUCKETS                                  
ACCHELD  DS    PL6                                                              
ACCBABLE DS    PL6                                                              
*                                                                               
NLEVELS  EQU   (*-CACBUCKS)/(NBUCKS*BUCKLN) NUMBER OF HIGHER LEVELS             
BUCKLN   EQU   6                                                                
*                                                                               
WCCNT    DS    PL4                 COUNT OF CACS PER WC                         
CACNT    DS    PL4                 COUNT OF TRANSACTIOS PER CAC                 
*                                                                               
OVERAMT  DS    PL6                                                              
OVEREST  DS    PL6                                                              
SORTAMNT DS    PL6                 TOTAL AMOUNT PUT TO SORT                     
PL13     DS    PL13                                                             
PKWORK   DS    PL6                                                              
CUREST#  DS    CL1                                                              
HIREV#   DS    CL1                                                              
ELCODE   DS    CL1                                                              
COMMAND  DS    CL8                                                              
TITLE    DS    CL30                                                             
TITLE2   DS    CL30                                                             
RUNTYPE  DS    CL9                                                              
JOBSTAT  DS    CL1                                                              
HASANEST EQU   1                                                                
NEEDSEST EQU   2                                                                
HASDRS   EQU   4                                                                
NOTBILED EQU   8                   FULLY BILLED, THAT IS                        
OVEST    EQU   16                                                               
T_1_OR_P EQU   32                  TOTAL, ONE LINE TOT OR PROGRESSIVE           
MARKED   EQU   80                  A TRANSACTION HAS BEEN HELD ON JOB           
*                                                                               
UNHOLDTP DS    CL1                                                              
UNHOLDDT DS    0CL6                                                             
UNHSTART DS    CL3                                                              
UNHEND   DS    CL3                                                              
*                                                                               
MYKEY    DS    CL(L'TRNKEY)                                                     
PREVTRN  DS    CL(L'TRNKEY)        PREVIOUS TRANSACTION                         
DCBKEY   DS    CL42                KEY FROM DCB                                 
ELEMENT  DS    CL255               SPACE TO BUILD AN ELEMENT                    
CLICODE  DS    CL6                                                              
CLINAME  DS    CL36                                                             
PROCODE  DS    CL6                                                              
PRONAME  DS    CL36                                                             
JOBCODE  DS    CL6                                                              
JOBNAME  DS    CL36                                                             
*                                                                               
JOBINFO  DS    0C                                                               
JBBLTYPE DS    CL25                                                             
JBFILTER DS    0CL5                                                             
JBF1     DS    CL1                                                              
JBF2     DS    CL1                                                              
JBF3     DS    CL1                                                              
JBF4     DS    CL1                                                              
JBF5     DS    CL1                                                              
*                                                                               
JBOPEN   DS    CL8                                                              
JBCLOSED DS    CL12                                                             
JBLASTBL DS    CL8                                                              
JBOFFICE DS    CL8                                                              
JBPOB    DS    CL(L'PPRBILLP)                                                   
JBLEN    EQU   *-JOBINFO                                                        
*                                                                               
LISTDATA DS    (LISTDLN)C                                                       
NMPTDATA DS    (NMPTDLN)C                                                       
         EJECT                                                                  
*----------------------------------------------------------------------         
*        THIS DSECT COVERS THE RECORD SENT TO QSORT                             
*----------------------------------------------------------------------         
TSORTD   DSECT                                                                  
TSSIGN   DS    CL1                 SIGN INDICATOR, TO MAINTAIN SORT SEQ         
NEGATIVE EQU   X'00'               SORT NEGS TO END OF DECENDING SORT           
POSITIVE EQU   X'FF'                                                            
TSKAMT   DS    PL6                 TRANSACTONMENT TRASNSACTION AMOUNT           
TSKEYLN  EQU   *-TSORTD            KEY LENGTH                                   
TSTRNKEY DS    CL(L'TRNKEY)        KEY OF THIS TRANSACTION                      
TSSEQ    DS    CL2                 HALFWORD SEQUENCE NUMBER                     
TSSTAT   DS    CL1                 REPORT STATUS                                
TSHELD   EQU   1                   TRANSACTION IS HELD                          
TSHDATE  DS    CL3                 HELD DATE                                    
TSHTYPE  DS    CL1                 I, M,  OR A                                  
TSORTLN  EQU   *-TSORTD                                                         
         EJECT                                                                  
*-------------------------------------------------------------------*           
* DSECT FOR MAIN TAB, A TABLE WHICH LOOPS THRU THE STORAGE GETMAIN              
*        GETS                                                                   
*-------------------------------------------------------------------*           
MAIND    DSECT                                                                  
MAINAST  DS    S                   ADDRESS TO STORE A(TABLE)                    
MAINTYPE DS    CL1                                                              
MAINBIN  EQU   C'B'                THIS IS A BINSRCH TABLE, SET HEADER          
         DS    CL1                                                              
MAINMAX  DS    A                   TO CALCULATE SIZE                            
MAINSIZE DS    A                                                                
MAINLEN  EQU   *-MAIND                                                          
         EJECT                                                                  
*-------------------------------------------------------------------*           
* DSECT FOR THE HEADER PORTION OF BINSRCH TABLES                    *           
*-------------------------------------------------------------------*           
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   REC LENGTH                                   
BINDISPK DS    0F                  DISP TO KEY                                  
BINDISP  DS    CL1                                                              
BINKEY   DS    CL3                                                              
BINMAX   DS    F                   MAX IN THIS TABLE                            
BINLENQ  EQU   *-BIND                                                           
BINTABLE DS    0CL1                TABLE DATA                                   
         EJECT                                                                  
*--------------------------------------------------------------------*          
* DSECT FOR BINSRCH NAME TABLES                                                 
*--------------------------------------------------------------------*          
NMPTD    DSECT                                                                  
NMPTKEY  DS    CL14                                                             
NMPTPTR  DS    CL4                                                              
NMPTDLN  EQU   *-NMPTD                                                          
         EJECT                                                                  
*--------------------------------------------------------------------*          
* DSECT TO PASS ACCOUNT/ACCOUNT NAMES AROUND                                    
*--------------------------------------------------------------------*          
LISTD    DSECT                                                                  
LISTREC  DS    0C                                                               
LISTKEY  DS    CL14                                                             
LISTNAME DS    CL36                                                             
LISTDLN  EQU   *-LISTD                                                          
         EJECT                                                                  
*--------------------------------------------------------------------*          
* DSECT FOR ACCOUNT LEVEL PRINT LIVE                                            
*--------------------------------------------------------------------*          
PACCD    DSECT                                                                  
PALEFT   DS    CL1                                                              
PAHIREV  DS    CL13                                                             
PACOL1   DS    CL1                                                              
PACUREST DS    CL13                                                             
PACOL2   DS    CL1                                                              
PAOVAMT  DS    CL13                                                             
PACOL3   DS    CL1                                                              
PAMAXEST DS    CL13                                                             
PACOL4   DS    CL1                                                              
PAACTUAL DS    CL13                                                             
PACOL5   DS    CL1                                                              
PABILLED DS    CL13                                                             
PACOL6   DS    CL1                                                              
PAHOLDTB DS    CL13                                                             
PACOL7   DS    CL1                                                              
PAHELD   DS    CL13                                                             
PACOL8   DS    CL1                                                              
PANEEDTB DS    CL13                                                             
PARIGHT  DS    CL1                                                              
PACLEN   EQU   *-PACCD                                                          
         ORG   PACCD+L'P+(PABILLED-PACCD)                                       
PABLABLE DS    CL13                                                             
         EJECT                                                                  
*--------------------------------------------------------------------*          
* DSECT FOR TRANSACTION LEVEL DETAIL                                            
*--------------------------------------------------------------------*          
PTRND    DSECT                                                                  
         DS    CL(TRREPOFF)                                                     
PTLEFT   DS    CL1                                                              
PTWC     DS    CL2                                                              
         DS    CL25                                                             
         ORG   PTWC                                                             
         DS    CL2                 INDENT VENDOR                                
PTVEND   DS    CL25                VENDOR CODE                                  
PTCOL1   DS    CL1                                                              
PTDATE   DS    CL8                                                              
PTCOL2   DS    CL1                                                              
PTAMNT   DS    CL13                                                             
PTCOL3   DS    CL1                                                              
PTHDATE  DS    CL8                                                              
PTCOL4   DS    CL1                                                              
PTHTYPE  DS    CL3                                                              
         DS    CL1                                                              
PTCOL5   DS    CL1                                                              
PTHELD1  DS    CL13                                                             
PTCOL6   DS    CL1                                                              
PTBABLE  DS    CL13                                                             
PTRIGHT  DS    CL1                                                              
         ORG   PTRND+L'P                                                        
         DS    CL(TRREPOFF)                                                     
         DS    CL1                                                              
         DS    CL4                                                              
PTVNAME  DS    CL20                                                             
         DS    CL1                                                              
         ORG   PTRND+L'P+(PTDATE-PTRND)                                         
PTREF    DS    CL6                 REF NUMBER UNDER DATE                        
LPTVNAME EQU   L'PTVNAME                                                        
TRREPOFF EQU   19                  OFFSET TO INDENT TRAN REPORT                 
         EJECT                                                                  
GOBLOCKD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACGOBLOCK                                                      
*                                                                               
         PRINT ON                                                               
*        ACREPWORK                                                              
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*        ACGENFILE                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*        ACGENMODES                                                             
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*        ACMASTD                                                                
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
*        DDMASTD                                                                
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*        DDREPXTRAD                                                             
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
*        DDBIGBOX                                                               
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*       DMDTFIS                                                                 
*********PRINT OFF                                                              
*********INCLUDE DMDTFIS                                                        
*********PRINT ON                                                               
*       ACJOBBERD                                                               
         PRINT OFF                                                              
       ++INCLUDE ACJOBBERD                                                      
         PRINT ON                                                               
JBLOCKD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACJOBBLOCK                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'099ACREPAH02 07/23/13'                                      
         END                                                                    
