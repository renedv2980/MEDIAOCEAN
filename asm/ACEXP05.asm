*          DATA SET ACEXP05    AT LEVEL 013 AS OF 01/27/04                      
*PHASE T61505A                                                                  
         TITLE 'T61505 - COKE EXPENDITURE - APPROVAL'                           
T61505   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (TEMPEND-TEMPD),T61505,R7,RR=R3                                  
         LR    R2,RC                                                            
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T615FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R6,SAVXTWA          R6=LOCAL SAVED STORAGE                       
         USING LWSD,R6                                                          
         ST    R3,RELO                                                          
         GOTO1 AUTH                                                             
         CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
         ST    R2,ATEMP           SAVE ADDRESS OF TEMP AREA                     
         USING TEMPD,R2                                                         
         MVI   SXCON,0                                                          
         CLI   TWAOFFC,C'*'                                                     
         BE    VALACN                                                           
         CLC   AGYSIGN(5),=C'CCUSA'                                             
         BE    VALACN                                                           
         CLC   AGYSIGN(5),=C'CCATA'                                             
         BNE   NOTAUTH                                                          
         EJECT                                                                  
*              INITIAL ROUTINES                                                 
         SPACE 1                                                                
*              VALIDATE ACN NUMBER                                              
         USING ACKEYD,R4                                                        
VALACN   MVI   KEYCHA,C'N'                                                      
         MVC   AIO,AIO1                                                         
         LA    R4,KEY                                                           
         MVC   KEY,SPACES                                                       
         MVC   ACKEYACC(1),COMPANY                                              
         MVC   ACKEYACC+1(2),=C'SE'                                             
         LA    R2,LOGACNH                                                       
         GOTO1 ANY                                                              
         MVC   ACKEYACC+3(5),WORK                                               
         TM    LOGACNH+4,X'20'                                                  
         BO    VALAGY                                                           
         MVI   KEYCHA,C'Y'                                                      
         MVC   LOGACNN,SPACES                                                   
         OI    LOGACNNH+6,X'80'                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVC   ACN,WORK                                                         
         GOTO1 NAMOUT,DMCB,AIO,WORK                                             
         MVC   LOGACNN,WORK                                                     
         OI    LOGACNH+4,X'20'                                                  
         SPACE 1                                                                
*              VALIDATE AGENCY                                                  
VALAGY   LA    R2,LOGAGYH                                                       
         GOTO1 ANY                                                              
         MVC   ACKEYACC+8(3),WORK                                               
         CLI   KEYCHA,C'Y'                                                      
         BE    *+12                                                             
         TM    LOGAGYH+4,X'20'                                                  
         BO    VALPRD                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVC   AGYCDE,WORK                                                      
         GOTO1 GETL,DMCB,(X'30',AIO),0                                          
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                NO STATUS ELEMENT                            
         L     R3,ELADDR                                                        
         USING ACSTATD,R3                                                       
         TM    ACSTSTAT,X'20'                                                   
         BO    ERR56               ACCOUNT IS LOCKED                            
         OI    LOGAGYH+4,X'20'                                                  
         MVI   KEYCHA,C'Y'                                                      
         SPACE 1                                                                
*              VALIDATE PRODUCT                                                 
VALPRD   LA    R2,LOGPRDH                                                       
         GOTO1 ANY                                                              
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),COMPANY                                              
         MVC   ACKEYACC+1(2),=C'3P'                                             
         MVC   ACKEYACC+3(2),WORK                                               
         CLI   KEYCHA,C'Y'                                                      
         BE    *+12                                                             
         TM    LOGPRDH+4,X'20'                                                  
         BO    VALMED                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVC   PRD,WORK                                                         
         MVC   PRDBUDC,=C'00'                                                   
         GOTO1 NAMOUT,DMCB,AIO,WORK                                             
         MVC   PRDNME,WORK                                                      
         GOTO1 GETL,DMCB,(X'23',AIO),0                                          
         CLI   ELERR,0                                                          
         BNE   VALPRD3             NO BUDGET CODE                               
         L     R3,ELADDR                                                        
         USING ACOTHERD,R3                                                      
         MVC   PRDBUDC,ACOTNUM     PRODUCT BUDGET CODE                          
VALPRD3  OI    LOGPRDH+4,X'20'                                                  
         MVI   KEYCHA,C'Y'                                                      
         SPACE 1                                                                
*              VALIDATE MEDIA                                                   
VALMED   LA    R2,LOGMEDH                                                       
         CLI   5(R2),0                                                          
         BNE   VALMED1                                                          
         CLC   MEDIA,SPACES                                                     
         BE    *+8                                                              
         MVI   KEYCHA,C'Y'                                                      
         OI    LOGMEDH+4,X'20'                                                  
         MVC   MEDIA,SPACES                                                     
         B     VALPERD                                                          
         SPACE 1                                                                
VALMED1  GOTO1 ANY                                                              
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),COMPANY                                              
         MVC   ACKEYACC+1(2),=C'3M'                                             
         MVC   ACKEYACC+3(2),WORK                                               
         CLI   KEYCHA,C'Y'                                                      
         BE    *+12                                                             
         TM    LOGMEDH+4,X'20'                                                  
         BO    VALPERD                                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVC   MEDIA,WORK                                                       
         OI    LOGMEDH+4,X'20'                                                  
         MVI   KEYCHA,C'Y'                                                      
         SPACE 1                                                                
*              VALIDATE PERIOD FIELD                                            
VALPERD  XC    STDATE,STDATE                                                    
         MVC   ENDATE,=3X'FF'                                                   
         LA    R2,LOGPERH                                                       
         TM    4(R2),X'20'                                                      
         BO    *+8                                                              
         MVI   KEYCHA,C'Y'                                                      
         CLI   5(R2),0                                                          
         BE    VALX                                                             
         GOTO1 SCANNER,DMCB,(R2),(1,BLOCK),C',=,-'                              
         CLI   DMCB+4,1                                                         
         BNE   FLDINV                                                           
         GOTO1 DATVAL,DMCB,(2,BLOCK+12),WORK                                    
         OC    DMCB(4),DMCB                                                     
         BZ    ERR13               INVALID DATE FORMAT                          
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,STDATE)                                  
         MVC   ENDATE,STDATE       IF ONLY ONE START = END                      
         CLI   BLOCK+1,0                                                        
         BE    VALX                NO SECOND DATE                               
         GOTO1 DATVAL,DMCB,(2,BLOCK+22),WORK                                    
         OC    DMCB(4),DMCB                                                     
         BZ    ERR13               INVALID DATE FORMAT                          
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,ENDATE)                                  
         CLC   STDATE,ENDATE       START DATE > END DATE                        
         BH    ERR13               YES,  INVALID DATE                           
         SPACE 1                                                                
VALX     OI    LOGPERH+4,X'20'                                                  
         EJECT                                                                  
*              ESTABLISH MODE                                                   
APP20    CLI   KEYCHA,C'Y'                                                      
         BE    APP21              KEY CHANGES - MUST DISPLAY                    
         CLI   APPMODE,1           ALREADY BUILT LIST OK TO APPROVE             
         BE    APP30                                                            
         SPACE 1                                                                
APP21    BAS   RE,CLRS                                                          
         XC    LASTKEY,LASTKEY                                                  
         XC    FRSTKEY,FRSTKEY                                                  
         MVI   DSPANY,C'N'                                                      
         BAS   RE,BLDLST           BUILD LIST OF UNAPPROVED INVOICES            
         CLI   INVCNT,0                                                         
         BE    NOMORE              NO INVOICES TO DISPLAY                       
         BAS   RE,DSPINV           DISPLAY INVOICES                             
         MVI   APPMODE,1                                                        
         B     DSPMSG                                                           
         SPACE 1                                                                
APP30    BAS   RE,APPINV           APPROVE INVOICES                             
         CLC   INVNXT,INVCNT                                                    
         BNH   APP32               NOT FINISHED LIST                            
         BAS   RE,BLDLST           BUILD NEXT LIST OF INVOICES                  
         CLI   INVCNT,0                                                         
         BE    NOMORE              NO MORE TO DISPLAY                           
APP32    BAS   RE,DSPINV           DISPLAY NEXT SET                             
         B     DSPMSG                                                           
         EJECT                                                                  
*              DISPLAY UNAPPROVED INVOICES                                      
         USING LINED,R2                                                         
BLDLST   NTR1                                                                   
         SPACE 1                                                                
         USING ITD,R5                                                           
         MVI   INVCNT,0            COUNT NUMBER IN TABLE                        
         MVI   INVNXT,1            NEXT ITEM TO BE DISPLAYED                    
         LA    R5,INVLST           CLEAR ITEM LIST                              
         LA    R0,MAXITM                                                        
BLDLST3  XC    ITMED(ITLEN),ITMED                                               
         ZAP   ITNET,=P'0'                                                      
         ZAP   ITGRS,=P'0'                                                      
         LA    R5,ITLEN(R5)                                                     
         BCT   R0,BLDLST3                                                       
         SPACE 1                                                                
         OC    FRSTKEY,FRSTKEY                                                  
         BNZ   BLDLST5                                                          
         LA    R4,FRSTKEY          BUILD FIRST KEY                              
         MVC   FRSTKEY,SPACES                                                   
         MVC   ACKEYACC(1),COMPANY                                              
         MVC   ACKEYACC+1(2),=C'SE'                                             
         MVC   ACKEYACC+3(5),ACN                                                
         MVC   ACKEYACC+8(3),AGYCDE                                             
         MVC   ACKEYCON(1),COMPANY                                              
         MVC   ACKEYCON+1(2),=C'3P'                                             
         MVC   ACKEYCON+3(2),PRD                                                
         MVC   ACKEYCON+5(2),MEDIA                                              
         MVC   LASTKEY,FRSTKEY                                                  
         SPACE 1                                                                
BLDLST5  MVC   KEY,SPACES                                                       
         MVC   KEY(L'LASTKEY),LASTKEY                                           
         LA    R5,INVLST                                                        
         USING ITD,R5                                                           
         LA    R0,MAXITM                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   LASTKEY,FRSTKEY     FIRST TIME DON'T SKIP LASTKEY                
         BE    BLDLST9                                                          
BLDLST7  MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
BLDLST9  L     R4,AIO                                                           
         MVC   LASTKEY,0(R4)                                                    
         CLC   ACKEYACC,FRSTKEY                                                 
         BNE   BLDLST11            NOT SAME ACCOUNT - END OF DISPLAY            
         LA    RF,FRSTKEY                                                       
         CLC   ACKEYCON(5),ACKEYCON-ACKEYD(RF)                                  
         BNE   BLDLST11            NOT SAME CONTRA                              
         CLC   MEDIA,SPACES                                                     
         BE    *+14                                                             
         CLC   ACKEYCON(7),ACKEYCON-ACKEYD(RF)                                  
         BNE   BLDLST11            NOT SAME CONTRA                              
         GOTO1 GETL,DMCB,(X'44',AIO),0                                          
         CLI   ELERR,0                                                          
         BNE   BLDLST7             NOT TRANSACTION GET NEXT                     
         OC    ACDTPEEL,ACDTPEEL                                                
         BNZ   BLDLST7             SKIP PEELED                                  
         L     R3,ELADDR                                                        
         USING TRANSD,R3                                                        
         TM    TRNSSTAT,X'20'      OFFSETTING                                   
         BO    BLDLST7                                                          
         CLC   TRNSDATE(2),STDATE                                               
         BL    BLDLST7             TRANSACTION DATE BEFORE START                
         CLC   TRNSDATE(2),ENDATE                                               
         BH    BLDLST7             TRANSACTION DATE AFTER END                   
         GOTO1 GETL,DMCB,(X'60',AIO),0                                          
         L     R3,ELADDR                                                        
         USING TRSTATD,R3                                                       
         OC    TRSTUPDT,TRSTUPDT                                                
         BNZ   BLDLST7                        IF DATE PRESENT - ALREADY         
         SPACE 1                                                                
*              ADD ITEM TO INVLST                                               
         MVC   ITMED,ACKEYCON+5    MEDIA                                        
         MVC   ITVEH,ACKEYCON+7    VEHICLE                                      
         MVC   ITDTE,ACKEYDTE      DATE                                         
         MVC   ITYRM,ACKEYDTE      YEAR/MONTH FOR SORT                          
         MVC   ITINV,ACKEYREF      INVOICE                                      
         MVC   ITSBR,ACKEYSBR                                                   
         SPACE 1                                                                
         GOTO1 GETL,DMCB,(X'50',(R4)),0                                         
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T FIND NET ELEMENT                       
         L     R3,ELADDR                                                        
         USING TRCASHD,R3                                                       
         ZAP   ITNET,TRCSNET         SAVE NET AMOUNT                            
         ZAP   ITGRS,TRCSGRS         AND GROSS AMOUNT                           
         SPACE 1                                                                
         ZIC   R1,INVCNT                                                        
         AH    R1,=H'1'                                                         
         STC   R1,INVCNT                                                        
         LA    R5,ITLEN(R5)        NEXT ITEM IN TABLE                           
         BCT   R0,BLDLST7                                                       
         SPACE 1                                                                
BLDLST11 CLI   INVCNT,2                                                         
         BL    BLDLSTX                                                          
         ZIC   R0,INVCNT                                                        
         LA    R2,ITLEN                                                         
         GOTO1 XSORT,DMCB,(0,INVLST),(R0),(R2),12,0                             
BLDLSTX  B     XIT                                                              
         EJECT                                                                  
*              DISPLAY ITEMS IN INVLST                                          
DSPINV   NTR1                                                                   
         BAS   RE,CLRS             CLEAR THE SCREEN                             
         SPACE 1                                                                
         USING ITD,R5                                                           
         SR    R0,R0                                                            
         ZIC   R1,INVNXT           NEXT ITEM                                    
         BCTR  R1,0                                                             
         LA    R5,ITLEN            LENGTH                                       
         MR    R0,R5                                                            
         LA    R5,INVLST(R1)       R5 TO NEXT ITEM IN TABLE                     
         SPACE 1                                                                
         ZIC   R0,INVCNT          TOTAL NUMBER OF ITEMS                         
         ZIC   R1,INVNXT                                                        
         BCTR  R1,0                                                             
         SR    R0,R1               R0 = NUMBER REMAINING                        
         LA    R3,MAXSCR           MAX ON SCREEN                                
         CR    R0,R3                                                            
         BL    *+6                                                              
         LR    R0,R3               R0 LOWER OF REMAINING OR MAX                 
         LA    R2,LOGDATAH                                                      
         USING LINED,R2                                                         
DSPINV5  MVC   LINMED,ITMED        MEDIA TO SCREEN                              
         GOTO1 DATCON,DMCB,(1,ITDTE),(6,WORK)                                   
         MVC   LINMTH(3),WORK      MMM                                          
         MVC   LINMTH+3(2),WORK+4  YY                                           
         MVC   LININV,ITINV        INVOICE NUMBER                               
         EDIT  (P6,ITNET),(10,LINNET),2,MINUS=YES                               
         EDIT  (P6,ITGRS),(10,LINGRS),2,MINUS=YES                               
         BAS   RE,VEHNAM           GET VEHICLE NAME                             
         MVC   LINVEH,WORK         NAME TO SCREEN                               
         BAS   RE,BUDGET           GET YTD AND BUDGET AMOUNTS                   
         CP    YTD,BUD                                                          
         BNH   *+8                                                              
         MVI   LINAPP,C'*'         INDICATE WHEN OVER BUDGET                    
         EDIT  (P6,BUD),(7,LINBUD),MINUS=YES,ZERO=NOBLANK                       
         CLI   QTRNUM,0                                                         
         BE    DSPINV7                                                          
         MVC   LINQTR(1),QTRNUM                                                 
         OI    LINQTR,X'F0'                                                     
         MVI   LINQTR+1,C'Q'                                                    
DSPINV7  EDIT  (P6,YTD),(8,LINTOT),MINUS=YES                                    
         BAS   RE,DSPMTOT          FIX MEDIA TOTAL LINES                        
         MVI   DSPANY,C'Y'                                                      
         LA    R5,ITLEN(R5)        NEXT ITEM IN TABLE                           
         LA    R2,LINLEN(R2)                                                    
         BCT   R0,DSPINV5                                                       
         B     XIT                                                              
         EJECT                                                                  
*              GET VEHICLE NAME                                                 
VEHNAM   NTR1                                                                   
         MVC   KEY,SPACES                                                       
         LA    R4,KEY                                                           
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),COMPANY                                              
         MVC   ACKEYACC+1(2),=C'3M'                                             
         MVC   ACKEYACC+3(2),ITMED                                              
         MVC   ACKEYACC+5(8),ITVEH                                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 NAMOUT,DMCB,AIO,WORK                                             
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY MEDIA TOTALS                                             
DSPMTOT  NTR1                                                                   
         LA    R3,LOGDATAH                                                      
         CR    R2,R3                                                            
         BE    DSPMTOTX            FIRST TIME LEAVE ALL DISPLAYED               
         LA    R4,ITLEN                                                         
         LNR   R4,R4               R5 HAS CURRENT ITEM IN INVLST                
         LA    R4,0(R4,R5)         R4 TO PREVIOUS ITEM IN INVLST                
         LA    R1,LINLEN                                                        
         LNR   R1,R1               R2 CURRENT LINE ON SCREEN                    
         LA    R1,0(R1,R2)         R1 TO PREVIOUS LINE ON SCREEN                
         CLC   ITMED(2),ITMED-ITD(R4)                                           
         BNE   DSPMTOTX            IF NOT SAME MEDIA LEAVE TOTALS               
         MVC   LINMED,SPACES       CLEAR CURRENT MEDIA                          
         CLC   ITYRM,ITYRM-ITD(R4)                                              
         BNE   *+10                IF SAME MONTH                                
         MVC   LINMTH,SPACES       CLEAR MONTH                                  
         CLC   ITQTR,ITQTR-ITD(R4)                                              
         BNE   DSPMTOTX            IF NOT SAME QUARTER LEAVE TOTALS             
         MVC   LINBUD-LINED(L'LINBUD,R1),SPACES    CLEAR PREVIOUS BUD           
         MVC   LINTOT-LINED(L'LINTOT,R1),SPACES    AND TOTAL                    
         MVC   LINQTR-LINED(L'LINQTR,R1),SPACES                                 
DSPMTOTX B     XIT                                                              
         EJECT                                                                  
*              GET YTD AND BUDGET AMOUNTS                                       
BUDGET   NTR1                                                                   
*              GET BUDGET PERIOD                                                
         ZAP   YTD,=P'0'                                                        
         ZAP   BUD,=P'0'                                                        
         MVC   BUDST(1),ITDTE                                                   
         MVI   BUDST+1,1                                                        
         MVC   BUDEN(1),ITDTE                                                   
         MVI   BUDEN+1,X'12'                                                    
         MVI   QTRNUM,0                                                         
         MVI   ITQTR,0                                                          
         CLC   ITMED,=C'ST'                                                     
         BNE   BUDGF                                                            
         LA    R1,BUDPER                                                        
BUDGA    CLI   0(R1),X'FF'                                                      
         BE    XIT                                                              
         CLC   0(1,R1),ITDTE+1  MONTH IN TABLE VS KEY MONTH                     
         BE    BUDGB                                                            
         LA    R1,4(R1)                                                         
         B     BUDGA                                                            
BUDGB    MVC   BUDST+1(1),1(R1)    START                                        
         MVC   BUDEN+1(1),2(R1)    END MONTHS FOR QUARTER                       
         MVC   QTRNUM,3(R1)        1,2,3,4 QUARTER                              
         MVC   ITQTR,3(R1)                                                      
         SPACE 1                                                                
*              BUILD KEY FOR CONTRA ACCOUNT                                     
         SPACE 1                                                                
BUDGF    LA    R4,KEY                                                           
         MVC   KEY,SPACES                                                       
         MVC   ACKEYACC(1),COMPANY                                              
         MVC   ACKEYACC+1(2),=C'SE'                                             
         MVC   ACKEYACC+3(5),ACN                                                
         MVC   ACKEYACC+8(3),AGYCDE                                             
         MVC   ACKEYCON(1),COMPANY                                              
         MVC   ACKEYCON+1(2),=C'3P'                                             
         MVC   ACKEYCON+3(2),PRD                                                
         MVC   ACKEYCON+5(2),ITMED                                              
         MVI   ACKEYREF,C'E'                                                    
         MVC   YEAR,ITDTE                                                       
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         L     R4,AIO                                                           
         CLC   KEYSAVE(ACLENGTH-ACKEYD),0(R4)                                   
         BNE   BUDG10                                                           
         LA    R3,ACRECORD         GET YTD FROM GROSS(DEBIT) BUCKETS            
BUDG3    CLI   0(R3),0                                                          
         BE    BUDG10                                                           
         CLI   0(R3),X'45'                                                      
         BE    BUDG7                                                            
BUDG5    ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     BUDG3                                                            
         SPACE 1                                                                
         USING TRHISTD,R3                                                       
BUDG7    CLC   TRHSYEAR(2),BUDST                                                
         BL    BUDG5                                                            
         CLC   TRHSYEAR(2),BUDEN                                                
         BH    BUDG5                                                            
         AP    YTD,TRHSDR                                                       
         B     BUDG5                                                            
BUDG10   AP    YTD,=P'50'                                                       
         ZAP   PWRK,YTD                                                         
         DP    PWRK,=P'100'                                                     
         ZAP   YTD,PWRK(11)                                                     
         SPACE 1                                                                
BUDGX    MVC   AIO,AIO1            RESET IO AREA                                
         BAS   RE,GTBUD                                                         
         B     XIT                                                              
         EJECT                                                                  
         DROP  R4                                                               
*              GET THE BUDGET AMOUNT                                            
         SPACE 1                                                                
GTBUD    NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING ACBTKEY,RF                                                       
         MVI   ACBTKTYP,ACBTKTEQ   BUILD BUDGET RECORD KEY                      
         MVC   ACBTKACC,SPACES                                                  
         MVC   ACBTKACC(1),COMPANY                                              
         MVC   ACBTKACC+1(2),=C'SE'                                             
         MVC   ACBTKACC+3(5),ACN                                                
         MVC   ACBTKACC+8(3),AGYCDE                                             
         MVC   ACBTKWRK,SPACES                                                  
         MVC   ACBTKCON,SPACES                                                  
         MVC   ACBTKCON(1),COMPANY                                              
         MVC   ACBTKCON+1(2),=C'3P'                                             
         MVC   ACBTKCON+3(2),PRDBUDC                                            
         MVC   ACBTKCON+5(2),ITMED                                              
         MVI   ACBTKBNO+1,1                                                     
         DROP  RF                                                               
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
         L     R4,AIO2                                                          
         CLC   KEYSAVE(ACLENGTH-ACKEYD),0(R4)                                   
         BNE   XIT                 NO BUDGET RECORD                             
         LA    R3,ACRECORD-ACKEYD(R4)                                           
GTBUD7   CLI   0(R3),0                                                          
         BE    GTBUD10                                                          
         CLI   0(R3),X'1D'                                                      
         BE    GTBUD9                                                           
GTBUD8   ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GTBUD7                                                           
         SPACE 1                                                                
         USING ACBAD,R3                                                         
GTBUD9   CLC   ACBAMNTH,BUDST      MUST BE WITHIN BUDGET PERIOD                 
         BL    GTBUD8                                                           
         CLC   ACBAMNTH,BUDEN                                                   
         BH    GTBUD8                                                           
         AP    BUD,ACBABUDG                                                     
         B     GTBUD8                                                           
         SPACE 1                                                                
GTBUD10  CP    BUD,=P'0'                                                        
         BE    XIT                                                              
         ZAP   PWRK,BUD                                                         
         DP    PWRK,=P'100'                                                     
         ZAP   BUD,PWRK(11)                                                     
         B     XIT                                                              
         EJECT                                                                  
*              APPROVE INVOICES                                                 
         USING LINED,R2                                                         
         USING ITD,R5                                                           
APPINV   NTR1                                                                   
         SR    R0,R0                                                            
         ZIC   R1,INVNXT           NEXT ITEM                                    
         BCTR  R1,0                                                             
         LA    R5,ITLEN            LENGTH                                       
         MR    R0,R5                                                            
         LA    R5,INVLST(R1)       R5 TO NEXT ITEM IN TABLE                     
         SPACE 1                                                                
         ZIC   R0,INVCNT          TOTAL NUMBER OF ITEMS                         
         ZIC   R1,INVNXT                                                        
         BCTR  R1,0                                                             
         SR    R0,R1               R0 = NUMBER REMAINING                        
         LA    R3,MAXSCR           MAX ON SCREEN                                
         CR    R0,R3                                                            
         BL    *+6                                                              
         LR    R0,R3               R0 LOWER OF REMAINING OR MAX                 
         SPACE 1                                                                
         BAS   RE,INITTRN          INITIALIZE ADDTRN BLOCK                      
         MVI   APPSW,C'N'          INITIALIZE APPROVED SWITCH                   
         LA    R2,LOGDATAH                                                      
APPINV2  CLI   LINAPPH+5,0                                                      
         BE    APPINV5                                                          
         CLI   LINAPP,C'N'                                                      
         BE    APPINV5             NOT APPROVED SKIP                            
         CLI   LINAPP,C'*'                                                      
         BE    APPINV5                                                          
         CLI   LINAPP,C'Y'                                                      
         BE    *+12                                                             
         LA    R2,LINAPPH                                                       
         B     FLDINV                                                           
         BAS   RE,APP              APPROVED UPDATE FILE                         
         MVI   APPSW,C'Y'                                                       
APPINV5  LA    R5,ITLEN(R5)                                                     
         LA    R2,LINLEN(R2)                                                    
         BCT   R0,APPINV2                                                       
         SPACE 1                                                                
         CLI   APPSW,C'Y'                                                       
         BNE   APPINV7                                                          
         MVI   TRNINDS,TRNILAST                                                 
         GOTO1 ADDTRN,TRNBLK                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
APPINV7  ZIC   R0,INVCNT          TOTAL NUMBER OF ITEMS                         
         ZIC   R1,INVNXT                                                        
         BCTR  R1,0                                                             
         SR    R0,R1               R0 = NUMBER REMAINING                        
         LA    R3,MAXSCR           MAX ON SCREEN                                
         CR    R0,R3                                                            
         BL    *+6                                                              
         LR    R0,R3               R0 LOWER OF REMAINING OR MAX                 
         ZIC   R1,INVNXT                                                        
         AR    R1,R0                                                            
         STC   R1,INVNXT           UPDATE NEXT ITEM TO DISPLAY                  
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO INITIALIZE ADDTRN BLOCK                                        
*                                                                               
INITTRN  NTR1  ,                                                                
         LA    RE,TRNBLK                                                        
         LA    RF,TRNBLKL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RE,ATIA                                                          
         L     RF,=A(14*1024)      CLEAR THE TIA                                
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   TRNCOMF,ACOMFACS                                                 
         MVC   TRNREC,AIO                                                       
         L     RE,ATIA             USE THE TIA FOR BUFFERS                      
         STCM  RE,15,TRNACC                                                     
         LA    RE,2048(RE)                                                      
         STCM  RE,15,TRNBUK                                                     
         LA    RE,2048(RE)                                                      
         STCM  RE,15,TRNCAC                                                     
         LA    RE,2048(RE)                                                      
         STCM  RE,15,TRNOFA                                                     
         LA    RE,2048(RE)                                                      
         ST    RE,ALEDGER                                                       
         LA    RE,LEDGTAB                                                       
         ST    RE,TRNLDG                                                        
         MVI   TRN#LDGS,1          PASS NUMBER OF LEDGER TABLE ENTRIES          
*                                                                               
         USING CPYELD,R3                                                        
         LA    R3,COMPEL                                                        
         MVC   TRNCPYS1,CPYSTAT1                                                
         MVC   TRNCPYS2,CPYSTAT2                                                
         MVC   TRNCPYS3,CPYSTAT3                                                
         MVC   TRNCPYS4,CPYSTAT4                                                
         CLI   CPYLN,CPYLN2Q       TEST LONG ELEMENT                            
         BL    INITT02                                                          
         MVC   TRNCPYS5,CPYSTAT5                                                
         MVC   TRNCPYS6,CPYSTAT6                                                
         MVC   TRNCPYS7,CPYSTAT7                                                
         MVC   TRNCPYS8,CPYSTAT8                                                
         CLI   CPYLN,CPYLN3Q                                                    
         BL    INITT02                                                          
         MVC   TRNCPYS9,CPYSTAT9                                                
         MVC   TRNCPYSA,CPYSTATA                                                
                                                                                
INITT02  LA    R0,PALAREA                                                       
         STCM  R0,15,TRNPAL                                                     
                                                                                
         MVC   TRNPUSER,TWAORIG                                                 
         MVC   TRNCACNM,PRDNME     C/A NAME=PRODUCT NAME                        
         OC    TRNCACNM,SPACES                                                  
         BAS   RE,GETLED           GET LEDGER TABLE ENTRY FOR SE                
         B     XIT                                                              
         EJECT                                                                  
*              SUB-ROUTINE TO GET A LEDGER RECORD AND BUILD A LEDGER            
*              TABLE ENTRY                                                      
*                                                                               
         USING ACKEYD,R4                                                        
GETLED   NTR1  ,                                                                
         MVC   SVKEY,KEY           SAVE THE KEY                                 
         LA    R5,LEDGTAB          R6=A(LEDGER TABLE ENTRY)                     
         LA    R4,KEY                                                           
         MVC   KEY(42),SPACES                                                   
         MVC   ACKEYACC(1),COMPANY GET THE SE LEDGER RECORD                     
         MVC   ACKEYACC+1(2),=C'SX'                                             
         MVC   AIO,ALEDGER                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   ACKEYACC,KEYSAVE                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,SVKEY           RESTORE KEY                                  
         MVC   AIO,AIO1            AND IO AREA POINTER                          
         L     R4,ALEDGER                                                       
*                                                                               
         USING LDGTABD,R5                                                       
GETLED2  XC    LDGTABD(LDGTABL),LDGTABD                                         
         MVC   LDGTUL,ACKEYACC+1                                                
         LA    R3,ACRECORD                                                      
         SR    R0,R0                                                            
*                                                                               
GETLED4  CLI   0(R3),0             TEST FOR EOR                                 
         BE    GETLEDX             YES                                          
         CLI   0(R3),ACLTELQ       TEST FOR LEDGER ELEMENT                      
         BNE   GETLED5             NO                                           
         USING ACLEDGD,R3                                                       
         MVC   LDGTTYPE,ACLTTYPE                                                
         MVC   LDGTLIKE,ACLTLIKE                                                
         MVC   LDGTOFFP,ACLTOFF                                                 
         B     GETLED8                                                          
*                                                                               
GETLED5  CLI   0(R3),ACHRELQ                                                    
         BNE   GETLED6                                                          
         USING ACHEIRD,R3                                                       
         MVC   LDGTLVA,ACHRLEVA                                                 
         MVC   LDGTLVB,ACHRLEVB                                                 
         MVC   LDGTLVC,ACHRLEVC                                                 
         MVC   LDGTLVD,ACHRLEVD                                                 
         B     GETLED8                                                          
*                                                                               
GETLED6  CLI   0(R3),ACSTELQ                                                    
         BNE   GETLED8                                                          
*                                                                               
         USING ACSTATD,R3                                                       
         MVC   LDGTSEC,ACSTSECY+1                                               
*                                                                               
GETLED8  IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GETLED4                                                          
*                                                                               
GETLEDX  B     XIT                                                              
         EJECT                                                                  
*              ADD APPROVED ITEM                                                
*              AT ENTRY, R5=A(INVLST ENTRY)                                     
*                                                                               
APP      NTR1                                                                   
         MVI   BOTSW,C'B'          ASSUME BOTTLER OR MC CANN BOUGHT             
         CLC   AGYCDE,=C'100'                                                   
         BL    APP2                                                             
         CLC   AGYCDE,=C'899'                                                   
         BH    APP2                                                             
         MVI   BOTSW,C'A'          AGENCY BOUGHT                                
APP2     MVC   KEY,SPACES                                                       
         LA    R4,KEY                                                           
         USING ACKEYACC,R4                                                      
         USING ITD,R5                                                           
*              BUILD TRANSACTION KEY                                            
         MVC   ACKEYACC(1),COMPANY                                              
         MVC   ACKEYACC+1(2),=C'SE'                                             
         MVC   ACKEYACC+3(5),ACN                                                
         MVC   ACKEYACC+8(3),AGYCDE                                             
         MVC   ACKEYCON(1),COMPANY                                              
         MVC   ACKEYCON+1(2),=C'3P'                                             
         MVC   ACKEYCON+3(2),PRD                                                
         MVC   ACKEYCON+5(2),ITMED                                              
         MVC   ACKEYCON+7(8),ITVEH                                              
         MVC   ACKEYREF,ITINV                                                   
         OC    ACKEYREF,SPACES                                                  
         MVC   ACKEYDTE,ITDTE                                                   
         MVC   ACKEYSBR,ITSBR                                                   
         GOTO1 HIGH                                                             
         SPACE 1                                                                
         L     R3,AIO                                                           
         CLC   KEYSAVE(ACLENGTH-ACKEYD),0(R3)                                   
         BNE   XIT             ITEM COULD HAVE BEEN DELETED SKIP IT             
         TM    ACSTATUS-ACKEYD(R3),X'80'                                        
         BO    XIT                                                              
         SPACE 1                                                                
         GOTO1 GETL,DMCB,(X'46',AIO),0                                          
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T FIND EXTRA PAY ELEMENT                 
         L     R3,ELADDR                                                        
         USING TRPAYD,R3                                                        
         ZAP   CSHD,TRPYCD                                                      
         SPACE 1                                                                
         GOTO1 GETL,DMCB,(X'50',AIO),0                                          
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T FIND NET ELEMENT                       
         L     R3,ELADDR                                                        
         USING TRCASHD,R3                                                       
         ZAP   DUB,TRCSNET         SAVE NET AMOUNT                              
         SPACE 1                                                                
         GOTO1 GETL,DMCB,(X'60',AIO),0                                          
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                NO 60 ELEMENT                                
         L     R3,ELADDR                                                        
         USING TRSTATD,R3                                                       
         OC    TRSTUPDT,TRSTUPDT                                                
         BNZ   XIT                 ALREADY APPROVED                             
         MVC   TRSTUPDT,TODAY2      ADD DATE APPROVED                           
         SPACE 1                                                                
         GOTO1 GETL,DMCB,(X'44',AIO),0                                          
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                NO 44 ELEMENT                                
         L     R3,ELADDR                                                        
         USING TRANSD,R3                                                        
         ZAP   TRNSAMNT,DUB        UPDATE AMOUNT                                
         SP    TRNSAMNT,CSHD       LESS CASH DISCOUNT                           
         EJECT                                                                  
*              WRITE SE TRANSACTION WITH UPDATED AMOUNT                         
         CLI   BOTSW,C'A'                                                       
         BE    APP4      AGENCY BOUGHT WRITE TRANSACTION WITH AMOUNT            
         ZAP   TRNSAMNT,=P'0'   BOTTLER OR MC CANN NO AMOUNT                    
APP4     GOTO1 WRITE                                                            
*        CLI   BOTSW,C'B'                                                       
*        BE    XIT                 BOTTLER BOUGHT - NO SX POSTINGS              
         SPACE 1                                                                
*              UPDATE BALANCE ELEMENT FOR SE                                    
         L     R4,AIO                                                           
         GOTO1 UPBAL,DMCB,TRNSAMNT                                              
         SPACE 1                                                                
*              ADD RECORD SX RECORD                                             
         L     R4,AIO                                                           
         MVC   ACKEYACC+1(2),=C'SX'   SX  AND IO                                
         LA    R3,KEY                                                           
         MVC   0(ACLENGTH-ACKEYD,R3),0(R4)     SET KEY = IO                     
         GOTO1 GETL,DMCB,(X'44',AIO),0                                          
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,ELADDR                                                        
         USING TRANSD,R3                                                        
         NI    TRNSSTAT,X'FF'-X'80'      CREDIT SX                              
*                                                                               
         LA    R3,ELEMENT                                                       
         USING MPYELD,R3           BUILD & ADD MANUAL PAYMENT ELEMENT           
         XC    MPYELD(MPYLNQ),MPYELD                                            
         MVI   MPYEL,MPYELQ                                                     
         MVI   MPYLN,MPYLNQ                                                     
         MVC   MPYNO,SPACES                                                     
         ZAP   MPYAMNT,=P'0'                                                    
         MVC   MPYBNK,SPACES                                                    
         GOTO1 ADDL,DMCB,AIO,ELEMENT                                            
*                                                                               
         MVC   AIO,AIO2            TEST IF RECORD ALREADY ON FILE               
APP6     GOTO1 HIGH                                                             
         CLC   KEY(42),KEYSAVE     NOT FOUND, OK TO ADD                         
         BNE   APP8                                                             
         SR    R1,R1               UP THE SUB REF                               
         IC    R1,ACKEYSBR         AND TRY AGAIN                                
         AH    R1,=H'1'                                                         
         STC   R1,ACKEYSBR                                                      
         LA    R3,KEY                                                           
         MVC   0(ACLENGTH-ACKEYD,R3),0(R4)     SET KEY = IO                     
         B     APP6                                                             
*                                                                               
APP8     MVC   AIO,AIO1                                                         
         GOTO1 ADD             ADD SX TRANSACTION                               
         SPACE 1                                                                
*              UPDATE BALANCE ELEMENT AND HISTORY BUCKETS FOR SX                
*        DELETE GROSS NET ELEMENT SO UPDATE ADDS BUCKET FOR TRNSAMNT            
*                                                                               
         ST    R4,TRNREC                                                        
         MVI   TRNINDS1,TRNIDNAT                                                
         MVI   BYTE,C'E'                                                        
         GOTO1 DELL,DMCB,(X'50',AIO),(1,BYTE)                                   
         GOTO1 ADDTRN,TRNBLK                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO UPDATE SE ACCOUNT BALANCE ELEMENT                              
* AT ENTRY, P1=A(TRANSACTION AMOUNT)                                            
*                                                                               
UPBAL    NTR1  ,                                                                
         L     R5,0(R1)            GET A(TRANSACTION AMOUNT)                    
         MVC   SVKEY,KEY           SAVE KEY FIELD                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'ACKEYACC),ACKEYACC COPY TRANS ACCOUNT KEY                  
         MVC   AIO,AIO2            USE IO2                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'ACKEYACC),KEYSAVE                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETL,DMCB,('ACSTELQ',AIO),0                                      
         CLI   ELERR,0             TEST STATUS ELEM FOUND                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,ELADDR                                                        
         USING ACSTATD,R3                                                       
         MVC   ACSTLAST,TODAY1                                                  
*                                                                               
         GOTO1 GETL,DMCB,('ACBLELQ',AIO),0                                      
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,ELADDR                                                        
         USING ACBALD,R3                                                        
         AP    ACBLDR,0(L'TRNSAMNT,R5) ADD TRANSACTION AMT TO DEBITS            
*                                                                               
         GOTO1 WRITE                                                            
         MVC   AIO,AIO1            RESTORE IO POINTER                           
         MVC   KEY,SVKEY                                                        
         B     XIT                                                              
         EJECT                                                                  
*              CLEAR THE SCREEN                                                 
CLRS     NTR1                                                                   
         USING LINED,R2                                                         
         LA    R2,LOGDATAH         CLEAR SCREEN                                 
         LA    R0,MAXSCR                                                        
CLRS2    MVC   LINMED(LINDLEN),SPACES                                           
         OI    LINH+6,X'80'                                                     
         MVC   LINAPP,SPACES                                                    
         OI    LINAPPH+6,X'80'                                                  
         LA    R2,LINLEN(R2)                                                    
         BCT   R0,CLRS2                                                         
         B     XIT                                                              
         EJECT                                                                  
*              EXIT ROUTINES                                                    
NOMORE   CLI   DSPANY,C'N'         NOTHING DISPLAYED                            
         BE    *+14                                                             
         MVC   CONHEAD(L'MSG3),MSG3                                             
         B     *+10                                                             
         MVC   CONHEAD(L'MSG2),MSG2                                             
         NI    LOGACNH+4,X'FF'-X'20'     NEXT TIME START AT FIRST               
         LA    R2,LOGACNH                                                       
         B     MYMSG                                                            
DSPMSG   MVC   CONHEAD(L'MSG1),MSG1                                             
         LA    R2,LOGAPPH                                                       
         B     MYMSG                                                            
NOTAUTH  MVC   CONHEAD(L'MSG4),MSG4                                             
         LA    R2,CONACTH                                                       
         B     MYMSG                                                            
MYMSG    MVI   ERROR,X'FE'                                                      
         B     THEEND                                                           
ERR13    MVI   ERROR,13            INVALID DATE FORMAT                          
         B     ACMESG                                                           
ERR56    MVI   ERROR,56            ACCOUNT IS LOCKED                            
         B     ACMESG                                                           
ACMESG   MVI   GETMSYS,6           ACCOUNT MESSAGES  (SYST 6)                   
         OI    GENSTAT2,USMYERSY                                                
         B     THEEND                                                           
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 EXIT                                                             
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS, LITERAL POOL, ETC.                                    
         SPACE 1                                                                
*        COMPLETION MESSAGES                                                    
MSG1     DC    C'INVOICES DISPLAYED - ENTER ''Y'' TO APPROVE'                   
MSG2     DC    C'ACTION COMPLETE - NO UNAPPROVED ITEMS TO DISPLAY'              
MSG3     DC    C'ACTION COMPLETE - NO MORE ITEMS TO DISPLAY'                    
MSG4     DC    C'*** ERROR *** ACTION NOT AUTHORIZED'                           
MAXSCR   EQU   15                                                               
MAXITM   EQU   100                                                              
         SPACE 1                                                                
*              SPOT MONTH                                                       
*              BUDGET START MONTH                                               
*              BUDGET END MONTH                                                 
*              QUARTER NUMBER                                                   
BUDPER   DC    X'01',X'0103',X'01'                                              
         DC    X'02',X'0103',X'01'                                              
         DC    X'03',X'0103',X'01'                                              
         DC    X'04',X'0406',X'02'                                              
         DC    X'05',X'0406',X'02'                                              
         DC    X'06',X'0406',X'02'                                              
         DC    X'07',X'0709',X'03'                                              
         DC    X'08',X'0709',X'03'                                              
         DC    X'09',X'0709',X'03'                                              
         DC    X'10',X'1012',X'04'                                              
         DC    X'11',X'1012',X'04'                                              
         DC    X'12',X'1012',X'04'                                              
         DC    X'FF'                                                            
         SPACE 1                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              LOCAL SAVED STORAGE                                              
         SPACE 1                                                                
LWSD     DSECT                                                                  
RELO     DS    F                                                                
ATEMP    DS    A                                                                
FRSTKEY  DS    CL42                                                             
LASTKEY  DS    CL42                                                             
YEAR     DS    CL1                                                              
YTD      DS    PL6                                                              
BUD      DS    PL6                                                              
CSHD     DS    PL6                                                              
PWRK     DS    PL13                                                             
         SPACE 1                                                                
BUDST    DS    CL2                                                              
BUDEN    DS    CL2                                                              
PRDBUDC  DS    CL2                                                              
QTRNUM   DS    CL1                                                              
         SPACE 1                                                                
BOTSW    DS    CL1                                                              
APPSW    DS    CL1                                                              
DSPANY   DS    CL1                 N=NOTHING DISPLAY                            
INVCNT   DS    CL1                 NUMBER IN TABLE                              
INVNXT   DS    CL1                 NUMBER OF THE NEXT ITEM TO DISPLAY           
         SPACE 1                                                                
APPMODE  DS    CL1                 X'00'=DISPLAY                                
*                                  X'01'=APPROVAL                               
KEYCHA   DS    CL1                 ANY KEY CHANGES 'Y' OR 'N'                   
ACN      DS    CL5                                                              
AGYCDE   DS    CL3                                                              
PRD      DS    CL2                                                              
MEDIA    DS    CL2                                                              
PRDNME   DS    CL36                                                             
STDATE   DS    CL3                                                              
ENDATE   DS    CL3                                                              
SVKEY    DS    CL42                                                             
LEDGTAB  DS    XL(LDGTABL+1)                                                    
ALEDGER  DS    A                                                                
         SPACE 1                                                                
       ++INCLUDE ACADDTRND                                                      
         SPACE 1                                                                
INVLST   DS    CL(ITLEN*MAXITM)                                                 
         SPACE 1                                                                
LWSLEN   EQU   *-LWSD              LENGTH OF SAVED STORAGE                      
         SPACE 2                                                                
*              DSECT FOR TEMP STORAGE                                           
TEMPD    DSECT                                                                  
SXCON    DS    0D                                                               
         DS    CL2048              SX CONTRA                                    
TEMPEND  EQU   *                                                                
         EJECT                                                                  
*              DSECT TO COVER SCREEN LINE                                       
         SPACE 1                                                                
LINED    DSECT                                                                  
LINH     DS    CL8                                                              
LINMED   DS    CL2                 MEDIA                                        
         DS    CL1                                                              
LINMTH   DS    CL5                 MONTH                                        
         DS    CL1                                                              
LINVEH   DS    CL20                VEHICLE                                      
         DS    CL1                                                              
LININV   DS    CL6                 INVOICE                                      
         DS    CL1                                                              
LINNET   DS    CL10                NET                                          
LINGRS   DS    CL10                GROSS                                        
LINTOT   DS    CL8                 TOTAL                                        
LINBUD   DS    CL7                 BUDGET                                       
         DS    CL1                                                              
LINQTR   DS    CL2                 QUARTER NUMBER                               
LINDLEN  EQU   *-LINMED                                                         
LINAPPH  DS    CL8                                                              
LINAPP   DS    CL2                 APPROVED                                     
LINLEN   EQU   *-LINED                                                          
         EJECT                                                                  
*              DSECT TO COVER ENTRY IN INVLST                                   
ITD      DSECT                                                                  
ITMED    DS    CL2                 MEDIA                                        
ITYRM    DS    CL2                 YEAR/MONTH FOR SORT                          
ITQTR    DS    CL1                 QUARTER NUMBER FOR SPOT                      
ITVEH    DS    CL8                 VEHICLE                                      
ITDTE    DS    CL3                 DATE                                         
ITINV    DS    CL6                 INVOICE NUMBER                               
ITSBR    DS    CL1                 SUB-REFERENCE                                
ITNET    DS    PL6                 NET                                          
ITGRS    DS    PL6                 GROSS                                        
ITLEN    EQU   *-ITD                                                            
         EJECT                                                                  
       ++INCLUDE ACEXPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACEXPFAD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
* ACEXPWORKD                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* ACGENBOTH                                                                     
* ACGENFILE                                                                     
* ACLDGTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACEXPWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACLDGTABD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACEXP05   01/27/04'                                      
         END                                                                    
