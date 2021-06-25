*          DATA SET ACEXP06    AT LEVEL 030 AS OF 01/27/04                      
*PHASE T61506A                                                                  
         TITLE 'T61506 - COKE EXPENDITURE - TRANSFER'                           
T61506   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (TEMPEND-TEMPD),T61506,R7,RR=R3                                  
         LR    R2,RC                                                            
         L     RC,0(,R1)           RC=GENCON STORAGE AREA                       
*                                                                               
         USING GEND,RC                                                          
*                                                                               
         L     RA,ATWA             RA=A(TWA)                                    
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
*                                                                               
         USING T615FFD,RA                                                       
         USING SYSD,R9                                                          
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R6,SAVXTWA          R6=LOCAL SAVED STORAGE                       
*                                                                               
         USING LWSD,R6                                                          
*                                                                               
         ST    R3,RELO                                                          
         GOTO1 AUTH                                                             
         CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
         ST    R2,ATEMP           SAVE ADDRESS OF TEMP AREA                     
*                                                                               
         USING TEMPD,R2                                                         
*                                                                               
         MVI   SXCON,0                                                          
         CLI   TWAOFFC,C'*'                                                     
         BE    VALACN                                                           
         CLC   AGYSIGN(5),=C'CCUSA'                                             
         BE    VALACN                                                           
         CLC   AGYSIGN(5),=C'CCATA'                                             
         BNE   NOTAUTH                                                          
         EJECT ,                                                                
*              INITIAL ROUTINES                                                 
         SPACE 1                                                                
*              VALIDATE ACN NUMBER                                              
         SPACE 1                                                                
         USING ACKEYD,R4                                                        
         SPACE 1                                                                
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
*                                                                               
         USING ACSTATD,R3                                                       
*                                                                               
         L     R3,ELADDR                                                        
         TM    ACSTSTAT,X'20'                                                   
         BO    ERR56               ACCOUNT IS LOCKED                            
*                                                                               
         OI    LOGAGYH+4,X'20'                                                  
         MVI   KEYCHA,C'Y'                                                      
         MVI   FRMTYPE,C'B'        BOTTLER BOUGHT                               
         CLC   AGYCDE,=C'100'      IF LESS THAN 100                             
         BL    VALPRD                                                           
         MVI   FRMTYPE,C'A'        AGENCY BOUGHT                                
         CLC   AGYCDE,=C'900'      IF LESS THAN 900                             
         BL    VALPRD                                                           
         MVI   FRMTYPE,C'M'        901-999 IS MCCANN                            
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
         GOTO1 NAMOUT,DMCB,AIO,WORK                                             
         MVC   PRDNME,WORK                                                      
         OI    LOGPRDH+4,X'20'                                                  
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
         BE    VALMED2                                                          
         TM    LOGMEDH+4,X'20'                                                  
         BO    VALPERD                                                          
*                                                                               
VALMED2  MVI   RDUPDATE,C'N'                                                    
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
         CLC   STDATE,ENDATE       START DATE <= END DATE ?                     
         BH    ERR13               NO,   INVALID DATE                           
         SPACE 1                                                                
VALX     OI    LOGPERH+4,X'20'                                                  
         EJECT ,                                                                
*              ESTABLISH MODE                                                   
         SPACE 1                                                                
TRN20    CLI   KEYCHA,C'Y'   DISPLAY                                            
         BE    TRN21         KEY CHANGES - MUST DISPLAY                         
         CLI   TRANMODE,1                                                       
         BE    TRN30         ALREADY BUILT LIST OK TO TRANSFER                  
         SPACE 1                                                                
TRN21    BAS   RE,CLRS                                                          
         XC    LASTKEY,LASTKEY                                                  
         XC    FRSTKEY,FRSTKEY                                                  
         MVC   LSTACN,SPACES                                                    
         MVI   DSPANY,C'N'                                                      
         BAS   RE,BLDLST           BUILD LIST OF INVOICES                       
         CLI   INVCNT,0                                                         
         BE    NOMORE              NO INVOICES TO DISPLAY                       
         BAS   RE,DSPINV           DISPLAY INVOICES                             
         MVI   TRANMODE,1                                                       
         B     DSPMSG                                                           
         SPACE 1                                                                
TRN30    BAS   RE,TRNINV           TRANSFER INVOICES                            
         CLC   INVNXT,INVCNT                                                    
         BNH   TRN32               NOT FINISHED LIST                            
         BAS   RE,BLDLST           BUILD NEXT LIST OF INVOICES                  
         CLI   INVCNT,0                                                         
         BE    NOMORE              NO MORE TO DISPLAY                           
         SPACE 1                                                                
TRN32    BAS   RE,DSPINV           DISPLAY NEXT SET                             
         B     DSPMSG                                                           
         EJECT ,                                                                
*              BUILD TABLE OF INVOICES                                          
         SPACE 1                                                                
         USING LINED,R2                                                         
         USING ITD,R5                                                           
         SPACE 1                                                                
BLDLST   NTR1                                                                   
         MVI   INVCNT,0            COUNT NUMBER IN TABLE                        
         MVI   INVNXT,1            NEXT ITEM TO BE DISPLAYED                    
         LA    R5,INVLST           CLEAR ITEM LIST                              
         LA    R0,MAXITM                                                        
*                                                                               
BLDLST3  XC    ITMED(ITLEN),ITMED                                               
         ZAP   ITAMT,=P'0'                                                      
         LA    R5,ITLEN(,R5)                                                    
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
         LA    R0,MAXITM                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   LASTKEY,FRSTKEY     FIRST TIME DON'T SKIP LASTKEY                
         BE    BLDLST9                                                          
         SPACE 1                                                                
BLDLST7  MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         SPACE 1                                                                
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
         TM    ACSTATUS,X'80'                                                   
         BO    BLDLST7             SKIP DELETED                                 
         L     R3,ELADDR                                                        
*                                                                               
         USING TRANSD,R3                                                        
*                                                                               
         TM    TRNSSTAT,X'20'                                                   
         BO    BLDLST7             SKIP OFFSETTING ITEMS                        
         CLC   TRNSDATE(2),STDATE                                               
         BL    BLDLST7             TRANSACTION DATE BEFORE START                
         CLC   TRNSDATE(2),ENDATE                                               
         BH    BLDLST7             TRANSACTION DATE AFTER END                   
         ZAP   SEAMT,TRNSAMNT                                                   
         MVC   ITSBRSE,ACKEYSBR    TRANSACTION SEQUENCE                         
*                                                                               
         MVC   KEY(L'LASTKEY),LASTKEY                                           
         MVI   KEY+41,0            READ FOR TRN WITH THE SAME $ AMOUNT          
         MVC   KEY+1(2),=C'SX'     MUST BE ON SX                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   ACKEYACC(41),KEYSAVE                                             
         BNE   BLDLST9E                                                         
BLDLST9B L     R4,AIO                                                           
         GOTO1 GETL,DMCB,(X'44',AIO),0                                          
         CLI   ELERR,0                                                          
         BNE   BLDLST9C                                                         
         L     R3,ELADDR                                                        
         USING TRANSD,R3                                                        
         CP    TRNSAMNT,SEAMT                                                   
         BE    BLDLST10                                                         
BLDLST9C GOTO1 SEQ                                                              
         CLC   KEY(41),KEYSAVE                                                  
         BE    BLDLST9B                                                         
                                                                                
BLDLST9E MVC   KEY(L'LASTKEY),LASTKEY   RESET SE KEY                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         B     BLDLST7             SKIP(NOT ON SX)                              
                                                                                
BLDLST10 DS    0H                                                               
         MVC   ITSBRSX,ACKEYSBR                                                 
         MVC   KEY(L'LASTKEY),LASTKEY RE-READ SE AND PROCESS                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
*                                                                               
*              ADD ITEM TO INVLST                                               
         MVC   ITMED,ACKEYCON+5    MEDIA                                        
         MVC   ITVEH,ACKEYCON+7    VEHICLE                                      
         MVC   ITDTE,ACKEYDTE      DATE                                         
         MVC   ITYRM,ACKEYDTE      YEAR/MONTH FOR SORT                          
         MVC   ITINV,ACKEYREF      INVOICE                                      
         SPACE 1                                                                
         GOTO1 GETL,DMCB,(X'50',(R4)),0                                         
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T FIND NET ELEMENT                       
*                                                                               
         USING TRCASHD,R3                                                       
*                                                                               
         L     R3,ELADDR                                                        
         ZAP   ITAMT,TRCSGRS       SAVE GROSS AMOUNT                            
         SPACE 1                                                                
         ZIC   R1,INVCNT                                                        
         LA    R1,1(,R1)                                                        
         STC   R1,INVCNT                                                        
         LA    R5,ITLEN(,R5)       NEXT ITEM IN TABLE                           
         BCT   R0,BLDLST7                                                       
         SPACE 1                                                                
BLDLST11 CLI   INVCNT,2                                                         
         BL    BLDLSTX                                                          
         ZIC   R0,INVCNT                                                        
         LA    R2,ITLEN                                                         
         GOTO1 XSORT,DMCB,(0,INVLST),(R0),(R2),12,0                             
         SPACE 1                                                                
BLDLSTX  B     XIT                                                              
         EJECT                                                                  
*              DISPLAY ITEMS IN INVLST                                          
         SPACE 1                                                                
DSPINV   NTR1                                                                   
         BAS   RE,CLRS             CLEAR THE SCREEN                             
*                                                                               
         USING ITD,R5                                                           
*                                                                               
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
*                                                                               
         USING LINED,R2                                                         
*                                                                               
DSPINV5  MVC   LINMED,ITMED        MEDIA TO SCREEN                              
         GOTO1 DATCON,DMCB,(1,ITDTE),(6,WORK)                                   
         MVC   LINMTH(3),WORK      MMM                                          
         MVC   LINMTH+3(2),WORK+4  YY                                           
         MVC   LININV,ITINV        INVOICE NUMBER                               
         EDIT  (P6,ITAMT),(10,LINAMT),2,MINUS=YES                               
         BAS   RE,VEHNAM           GET VEHICLE NAME                             
         MVC   LINVEH,WORK         NAME TO SCREEN                               
         GOTO1 =A(GETYTD),DMCB,(RC),RR=RELO  GET YEAR TO DATE                   
         SPACE 1                                                                
DSPINV7  EDIT  (P6,YTD),(10,LINTOT),MINUS=YES                                   
         BAS   RE,DSPMTOT          FIX MEDIA TOTAL LINES                        
         MVI   DSPANY,C'Y'                                                      
         LA    R5,ITLEN(,R5)       NEXT ITEM IN TABLE                           
         LA    R2,LINLEN(,R2)                                                   
         BCT   R0,DSPINV5                                                       
         B     XIT                                                              
         EJECT ,                                                                
*              GET VEHICLE NAME                                                 
         SPACE 1                                                                
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
         EJECT ,                                                                
*              DISPLAY MEDIA TOTALS                                             
         SPACE 1                                                                
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
         MVC   LINTOT-LINED(L'LINTOT,R1),SPACES    AND TOTAL                    
         SPACE 1                                                                
DSPMTOTX B     XIT                                                              
         EJECT ,                                                                
*              TRANSFER INVOICES                                                
         SPACE 1                                                                
         USING LINED,R2                                                         
         USING ITD,R5                                                           
         SPACE 1                                                                
TRNINV   NTR1                                                                   
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
         BAS   RE,VALTOACC         VALIDATE TO ACCOUNTS                         
         BAS   RE,INITTRN          INITIALIZE ADDTRN BLOCK                      
         SPACE 1                                                                
         LA    R2,LOGDATAH                                                      
         MVI   TRANSW,C'N'         SET SWITCH TO NOTE A TRANSFER                
         SPACE 1                                                                
TRNINV2  CLI   LINTRNH+5,0                                                      
         BE    TRNINV5                                                          
         MVC   LDGR,=C'SE'                                                      
         MVC   MYSBR,ITSBRSE       SEQUENCE NUMBER ON SE                        
         BAS   RE,TRN              ADD NEW                                      
         BAS   RE,REV              REVERSE OLD                                  
         MVC   LDGR,=C'SX'                                                      
         MVC   MYSBR,ITSBRSX       SEQUENCE NUMBER ON SX                        
         BAS   RE,TRN              ADD NEW                                      
         BAS   RE,REV              REVERSE OLD                                  
         MVI   TRANSW,C'Y'                                                      
         SPACE 1                                                                
TRNINV5  LA    R5,ITLEN(,R5)                                                    
         LA    R2,LINLEN(,R2)                                                   
         BCT   R0,TRNINV2                                                       
         SPACE 1                                                                
         CLI   TRANSW,C'Y'         TEST FOR AT LEAST ONE TRANSFER               
         BNE   TRNINV7             NO                                           
         OI    TRNINDS,TRNILAST                                                 
         GOTO1 ADDTRN,TRNBLK                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
TRNINV7  ZIC   R0,INVCNT          TOTAL NUMBER OF ITEMS                         
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
*              VALIDATE TO ACCOUNT                                              
         SPACE 1                                                                
         USING LINED,R2                                                         
         USING ITD,R5                                                           
         SPACE 1                                                                
VALTOACC NTR1                                                                   
         LA    R2,LOGDATAH                                                      
         LA    R2,LINTRNH                                                       
         MVC   LSTACN,SPACES                                                    
         SPACE 1                                                                
VALTO1   CLI   5(R2),0                                                          
         BE    VALTO9                NOT TRANSFERRING                           
         CLI   5(R2),3                                                          
         BNE   VALTO3                                                           
         MVC   ITACN,ACN           SAME AS BOTTLER                              
         MVC   ITAGY,8(R2)         TO AGENCY                                    
         B     VALTO5                                                           
         SPACE 1                                                                
VALTO3   CLI   5(R2),8                                                          
         BNE   VALTO4                                                           
         MVC   ITACN,8(R2)         TO BOTTLER                                   
         MVC   ITAGY,13(R2)        AND AGENCY                                   
         B     VALTO5                                                           
         SPACE 1                                                                
VALTO4   CLI   5(R2),1                                                          
         BNE   FLDINV                                                           
         CLI   8(R2),C'*'                                                       
         BNE   FLDINV                                                           
         CLC   LSTACN,SPACES                                                    
         BE    FLDINV                                                           
         MVC   ITACN(8),LSTACN     DITTO USE LAST ACN                           
         SPACE 1                                                                
VALTO5   MVI   TOTYPE,C'B'         BOTTLER BOUGHT                               
         CLC   ITAGY,=C'100'       IF LESS THAN 100                             
         BL    VALTO7                                                           
         MVI   TOTYPE,C'A'         AGENCY BOUGHT                                
         CLC   ITAGY,=C'900'       IF LESS THAN 900                             
         BL    VALTO7                                                           
         MVI   TOTYPE,C'M'         901-999 IS MCCANN                            
         SPACE 1                                                                
VALTO7   CLC   FRMTYPE,TOTYPE                                                   
         BNE   INVTYPE                                                          
         CLC   ACN(8),ITACN                                                     
         BE    FLDINV                                                           
*                                                                               
         USING ACKEYD,R4                                                        
*                                                                               
         LA    R4,KEY                                                           
         MVC   KEY,SPACES                                                       
         MVC   ACKEYACC(1),COMPANY                                              
         MVC   ACKEYACC+1(2),=C'SE'                                             
         MVC   ACKEYACC+3(5),ITACN    BOTTLER                                   
         MVC   ACKEYACC+8(3),ITAGY    AGENCY                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVC   LSTACN,ITACN                                                     
         GOTO1 GETL,DMCB,(X'32',AIO),0                                          
         CLI   ELERR,0                                                          
         BNE   FLDINV              NO BALANCE ELEMENT                           
         GOTO1 GETL,DMCB,(X'30',AIO),0                                          
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                NO STATUS ELEMENT                            
*                                                                               
         USING ACSTATD,R3                                                       
*                                                                               
         L     R3,ELADDR                                                        
         TM    ACSTSTAT,X'20'                                                   
         SPACE 1                                                                
         BO    ERR56               ACCOUNT IS LOCKED                            
         CLI   TOTYPE,C'A'                                                      
         BNE   VALTO9                                                           
         MVC   ACKEYACC+1(2),=C'SX'  MUST HAVE SX FOR AGENCY                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         SPACE 1                                                                
VALTO9   LA    R2,LINLEN(,R2)                                                   
         LA    R5,ITLEN(,R5)                                                    
         BCT   R0,VALTO1                                                        
         B     XIT                                                              
         EJECT ,                                                                
* SUB-ROUTINE TO INITIALIZE ADDTRN BLOCK                                        
         SPACE 1                                                                
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
         LA    RE,2048(,RE)                                                     
         STCM  RE,15,TRNBUK                                                     
         LA    RE,2048(,RE)                                                     
         STCM  RE,15,TRNCAC                                                     
         LA    RE,2048(,RE)                                                     
         STCM  RE,15,TRNOFA                                                     
         LA    RE,2048(,RE)                                                     
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
         DROP  R3                                                               
*                                                                               
         USING COMFACSD,R3                                                      
         L     R3,ACOMFACS                                                      
         GOTO1 CGETFACT,DMCB,(X'80',0),F#UTLD                                   
         L     R1,0(,R1)                                                        
         USING F@UTLD,R1                                                        
         TM    F@TTEST,X'80'       TEST UPD=NO                                  
         BNO   *+8                                                              
         OI    TRNINDS,TRNIWRNO    SET WRITE=NO                                 
*                                                                               
         GOTO1 CGETFACT,DMCB,(X'80',0),F#SELISD                                 
         L     R1,0(R1)                                                         
         USING F@SELISD,R1                                                      
         TM    F@SEIND,X'04'       TEST READ ONLY                               
         BNO   *+8                                                              
         OI    TRNINDS,TRNIWRNO    SET WRITE=NO                                 
*                                                                               
         DROP  R1,R3                                                            
*                                                                               
         BAS   RE,GETLED           GET LEDGER TABLE ENTRY FOR SE                
         B     XIT                                                              
         EJECT ,                                                                
*              SUB-ROUTINE TO GET A LEDGER RECORD AND BUILD A LEDGER            
*              TABLE ENTRY                                                      
         SPACE 1                                                                
         PUSH  USING                                                            
         SPACE 1                                                                
GETLED   NTR1  ,                                                                
         MVC   SVKEY,KEY           SAVE THE KEY                                 
         LA    R5,LEDGTAB          R6=A(LEDGER TABLE ENTRY)                     
         LA    R4,KEY                                                           
         MVC   KEY(42),SPACES                                                   
         MVC   ACKEYACC(1),COMPANY GET THE SE LEDGER RECORD                     
         MVC   ACKEYACC+1(2),=C'SE'                                             
         MVC   AIO,ALEDGER                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   ACKEYACC,KEYSAVE                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
         MVC   AIO,AIO1            AND IO AREA POINTER                          
         L     R4,ALEDGER                                                       
*                                                                               
         USING LDGTABD,R5                                                       
*                                                                               
GETLED2  XC    LDGTABD(LDGTABL),LDGTABD                                         
         MVC   LDGTUL,ACKEYACC+1                                                
         LA    R3,ACRECORD                                                      
         SR    R0,R0                                                            
*                                                                               
GETLED4  CLI   0(R3),0             TEST FOR EOR                                 
         BE    GETLEDX             YES                                          
         CLI   0(R3),ACLTELQ       TEST FOR LEDGER ELEMENT                      
         BNE   GETLED5             NO                                           
*                                                                               
         USING ACLEDGD,R3                                                       
*                                                                               
         MVC   LDGTTYPE,ACLTTYPE                                                
         MVC   LDGTLIKE,ACLTLIKE                                                
         MVC   LDGTOFFP,ACLTOFF                                                 
         B     GETLED8                                                          
*                                                                               
GETLED5  CLI   0(R3),ACHRELQ                                                    
         BNE   GETLED6                                                          
*                                                                               
         USING ACHEIRD,R3                                                       
*                                                                               
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
*                                                                               
         MVC   LDGTSEC,ACSTSECY+1                                               
*                                                                               
GETLED8  IC    R0,1(,R3)                                                        
         AR    R3,R0                                                            
         B     GETLED4                                                          
*                                                                               
GETLEDX  B     XIT                                                              
         POP   USING                                                            
*                                                                               
         EJECT ,                                                                
*              REVERSE OLD ITEM                                                 
         SPACE 1                                                                
         USING ACKEYD,R4                                                        
         SPACE 1                                                                
REV      NTR1                                                                   
         LA    R4,KEY                                                           
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),COMPANY BUILD TRANSACTION KEY                        
         MVC   ACKEYACC+1(2),LDGR                                               
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
         MVC   ACKEYSBR,MYSBR                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         L     R3,AIO                                                           
         CLC   KEYSAVE(42),0(R3)                                                
         BE    *+6                                                              
         DC    H'0'                CAN'T FIND ITEM I BUT IN TABLE               
*                                                                               
         GOTO1 GETL,DMCB,(X'44',AIO),0                                          
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                NO 44 ELEMENT                                
*                                                                               
         USING TRANSD,R3                                                        
*                                                                               
         L     R3,ELADDR                                                        
         MP    TRNSAMNT,=P'-1'     REVERSE TRANAACTION AMOUNT                   
         SPACE 1                                                                
         GOTO1 GETL,DMCB,(X'50',AIO),0                                          
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T FIND NET ELEMENT                       
*                                                                               
         USING TRCASHD,R3                                                       
*                                                                               
         L     R3,ELADDR                                                        
         MP    TRCSNET,=P'-1'      REVERSE NET AND GROSS                        
         MP    TRCSGRS,=P'-1'                                                   
         SPACE 1                                                                
         GOTO1 GETL,DMCB,(X'46',AIO),0                                          
         CLI   ELERR,0                                                          
         BNE   REV5                                                             
*                                                                               
         USING TRPAYD,R3                                                        
*                                                                               
         L     R3,ELADDR                                                        
         MP    TRPYCD,=P'-1'       REVERSE CASH DISCOUNT                        
         SPACE 1                                                                
REV5     GOTO1 GETL,DMCB,(X'60',AIO),0                                          
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                NO 60 ELEMENT                                
*                                                                               
         USING TRSTATD,R3                                                       
*                                                                               
         L     R3,ELADDR                                                        
         MVC   TRSTDATE,TODAY2     ADDED                                        
         MVC   TRSTUPDT,TODAY2     AND APPROVED TODAY                           
         CLC   LDGR,=C'SX'                                                      
         BNE   REV6                                                             
****     BNE   *+16                                                             
         XC    TRSTUDAT,TRSTUDAT   CLEAR USED DATE                              
****     XC    TRSTUPDT,TRSTUPDT   APPROVED                                     
*                                                                               
REV6     MVI   REVSW,C'Y'      IF OLD NOT APPROVED MARK AS REVERSAL             
         OC    TRSTUPDT,TRSTUPDT                                                
         BZ    REV7                 IF OLD IS APPROVED                          
         MVC   TRSTUPDT,TODAY2      THIS IA APPROVED TODAY                      
         MVI   REVSW,C'N'           DON'T MARK AS REVERSAL                      
         SPACE 1                                                                
REV7     L     R4,AIO                                                           
         ST    R4,TRNREC             A(RECORD)                                  
         MVI   TRNINDS1,0                                                       
         CLI   REVSW,C'Y'          TEST TO MARK REVERSALS                       
         BE    *+8                 YES                                          
         OI    TRNINDS1,TRNIDNMR   NO-DO NOT MARK REVERSALS                     
         GOTO1 ADDTRN,TRNBLK                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   LDGR,=C'SX'         NO E BUCKETS ON SX                           
         BE    XIT                                                              
         SPACE 1                                                                
*              NOW UPDATE TYPE 'E' BUCKETS                                      
         OI    TRNINDS1,TRNIDNAT+TRNIDNUB+TRNIDNMR+TRNIDNAD                     
         MVC   ACKEYCON+7(8),SPACES  CONTRA=PRODUCT/MEDIA                       
         GOTO1 GETL,DMCB,('SCIELQ',(R4)),0                                      
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING SCIELD,R3                                                        
*                                                                               
         L     R3,ELADDR                                                        
         MVI   SCITYPE,SCITCOKE    FUDGE TO ADD COKE ELEMENT                    
         GOTO1 ADDTRN,TRNBLK                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   SCITYPE,SCITGRNT    RESTORE ELEMENT TYPE                         
         B     XIT                                                              
         EJECT ,                                                                
*              ADD TRANSFERRED ITEM                                             
         SPACE 1                                                                
         USING ACKEYD,R4                                                        
         SPACE 1                                                                
TRN      NTR1                                                                   
         LA    R4,KEY                                                           
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),COMPANY     BUILD TRANSACTION KEY                    
         MVC   ACKEYACC+1(2),LDGR                                               
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
         MVC   ACKEYSBR,MYSBR                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         L     R3,AIO                                                           
         CLC   KEYSAVE(42),0(R3)                                                
         BE    *+6                                                              
         DC    H'0'                CAN'T FIND ITEM I BUT IN TABLE               
*                                                                               
         L     R4,AIO                                                           
         MVC   ACKEYACC+3(5),ITACN   SET NEW BOTTLER                            
         MVC   ACKEYACC+8(3),ITAGY   AND AGENCY                                 
         MVI   ACKEYSBR,0                                                       
         SPACE 1                                                                
         GOTO1 GETL,DMCB,(X'60',AIO),0                                          
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                NO 60 ELEMENT                                
*                                                                               
         USING TRSTATD,R3                                                       
*                                                                               
         L     R3,ELADDR                                                        
         MVC   TRSTDATE,TODAY2           ADDED                                  
         MVC   TRSTUPDT,TODAY2           AND APPROVED TODAY                     
         CLC   LDGR,=C'SX'                                                      
         BNE   *+10                                                             
*****    BNE   *+16                                                             
         XC    TRSTUDAT,TRSTUDAT   CLEAR USED DATE                              
*****    XC    TRSTUPDT,TRSTUPDT         APPROVED                               
*                                                                               
         MVI   REVSW,C'Y'                                                       
         OC    TRSTUPDT,TRSTUPDT   IF NOT APPROVED MARK AS REVERSAL             
         BZ    TRN5                 IF OLD IS APPROVED                          
         MVC   TRSTUPDT,TODAY2      THIS IA APPROVED TODAY                      
         MVI   REVSW,C'N'                                                       
         SPACE 1                                                                
TRN5     ST    R4,TRNREC             A(RECORD)                                  
         MVI   TRNINDS1,0                                                       
         CLI   REVSW,C'Y'          TEST TO MARK REVERSALS                       
         BE    *+8                                                              
         OI    TRNINDS1,TRNIDNMR                                                
         GOTO1 ADDTRN,TRNBLK                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   LDGR,=C'SX'         NO E BUCKETS ON SX                           
         BE    XIT                                                              
*                                                                               
*              NOW UPDATE TYPE 'E' BUCKETS                                      
*                                                                               
         OI    TRNINDS1,TRNIDNAT+TRNIDNUB+TRNIDNMR+TRNIDNAD                     
         MVC   ACKEYCON+7(8),SPACES   C/A=PRODUCT/MEDIA                         
         GOTO1 GETL,DMCB,('SCIELQ',(R4)),0                                      
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING SCIELD,R3                                                        
*                                                                               
         L     R3,ELADDR                                                        
         MVI   SCITYPE,SCITCOKE    FUDGE TO ADD COKE ELEMENT                    
         GOTO1 ADDTRN,TRNBLK                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   SCITYPE,SCITGRNT    RESTORE ELEMENT TYPE                         
         B     XIT                                                              
         EJECT ,                                                                
*              CLEAR THE SCREEN                                                 
         SPACE 1                                                                
         USING LINED,R2                                                         
         SPACE 1                                                                
CLRS     NTR1                                                                   
         LA    R2,LOGDATAH         CLEAR SCREEN                                 
         LA    R0,MAXSCR                                                        
         SPACE 1                                                                
CLRS2    MVC   LINMED(LINDLEN),SPACES                                           
         OI    LINH+6,X'80'                                                     
         MVC   LINTRN,SPACES                                                    
         OI    LINTRNH+6,X'80'                                                  
         LA    R2,LINLEN(,R2)                                                   
         BCT   R0,CLRS2                                                         
         B     XIT                                                              
         EJECT ,                                                                
*              EXIT ROUTINES                                                    
         SPACE 1                                                                
NOMORE   CLI   DSPANY,C'N'         NOTHING DISPLAYED                            
         BE    NOMORE1                                                          
         MVC   CONHEAD(L'MSG3),MSG3                                             
         B     NOMORE2                                                          
         SPACE 1                                                                
NOMORE1  MVC   CONHEAD(L'MSG2),MSG2                                             
         SPACE 1                                                                
NOMORE2  NI    LOGACNH+4,X'FF'-X'20'     NEXT TIME START AT FIRST               
         LA    R2,LOGACNH                                                       
         MVI   TRANMODE,0                                                       
         B     MYMSG                                                            
         SPACE 1                                                                
DSPMSG   MVC   CONHEAD(L'MSG1),MSG1                                             
         LA    R2,LOGTRNH                                                       
         B     MYMSG                                                            
         SPACE 1                                                                
NOTAUTH  MVC   CONHEAD(L'MSG4),MSG4                                             
         LA    R2,CONACTH                                                       
         B     MYMSG                                                            
         SPACE 1                                                                
INVTYPE  MVC   CONHEAD(L'MSG5),MSG5                                             
         B     MYMSG                                                            
         SPACE 1                                                                
MYMSG    MVI   ERROR,X'FE'                                                      
         B     THEEND                                                           
         SPACE 1                                                                
ERR13    MVI   ERROR,13            INVALID DATE FORMAT                          
         B     ACMESG                                                           
         SPACE 1                                                                
ERR56    MVI   ERROR,56            ACCOUNT IS LOCKED                            
*        B     ACMESG                                                           
         SPACE 1                                                                
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
         EJECT ,                                                                
*              CONSTANTS, LITERAL POOL, ETC.                                    
         SPACE 1                                                                
*        COMPLETION MESSAGES                                                    
MSG1     DC    C'INVOICES DISPLAYED - ENTER TO ACCOUNT'                         
MSG2     DC    C'ACTION COMPLETE - NO ITEMS TO DISPLAY'                         
MSG3     DC    C'ACTION COMPLETE - NO MORE ITEMS TO DISPLAY'                    
MSG4     DC    C'*** ERROR *** ACTION NOT AUTHORIZED'                           
MSG5     DC    C'*** ERROR *** AGENCY TYPE MUST MATCH FROM AGENCY'              
         SPACE 1                                                                
MAXSCR   EQU   15                                                               
MAXITM   EQU   100                                                              
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
*              GET YTD                                                          
         SPACE 1                                                                
GETYTD   NMOD1 0,*YTD*                                                          
         L     RC,0(,R1)                                                        
*              GET BUDGET PERIOD                                                
         ZAP   YTD,=P'0'                                                        
         MVC   BUDST(1),ITDTE                                                   
         MVI   BUDST+1,1                                                        
         MVC   BUDEN(1),ITDTE                                                   
         MVI   BUDEN+1,X'12'                                                    
         MVI   QTRNUM,0                                                         
         MVI   ITQTR,0                                                          
         CLC   ITMED,=C'ST'                                                     
         BNE   BUDGF                                                            
         LA    R1,BUDPER                                                        
         SPACE 1                                                                
BUDGA    CLI   0(R1),X'FF'                                                      
         BE    BUDGX                                                            
         CLC   0(1,R1),ITDTE+1  MONTH IN TABLE VS KEY MONTH                     
         BE    BUDGB                                                            
         LA    R1,4(,R1)                                                        
         B     BUDGA                                                            
         SPACE 1                                                                
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
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         L     R4,AIO                                                           
         CLC   KEYSAVE(ACLENGTH-ACKEYD),0(R4)                                   
         BNE   BUDG10                                                           
         LA    R3,ACRECORD         GET YTD FROM GROSS(DEBIT) BUCKETS            
         SPACE 1                                                                
BUDG3    CLI   0(R3),0                                                          
         BE    BUDG10                                                           
         CLI   0(R3),X'45'                                                      
         BE    BUDG7                                                            
         SPACE 1                                                                
BUDG5    ZIC   R0,1(,R3)                                                        
         AR    R3,R0                                                            
         B     BUDG3                                                            
*                                                                               
         USING TRHISTD,R3                                                       
*                                                                               
BUDG7    CLC   TRHSYEAR(2),BUDST                                                
         BL    BUDG5                                                            
         CLC   TRHSYEAR(2),BUDEN                                                
         BH    BUDG5                                                            
         AP    YTD,TRHSDR                                                       
         B     BUDG5                                                            
         SPACE 1                                                                
BUDG10   AP    YTD,=P'50'                                                       
         ZAP   PWRK,YTD                                                         
         DP    PWRK,=P'100'                                                     
         ZAP   YTD,PWRK(11)                                                     
         SPACE 1                                                                
BUDGX    MVC   AIO,AIO1            RESET IO AREA                                
         XIT1                                                                   
         SPACE 1                                                                
         LTORG                                                                  
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
         EJECT ,                                                                
*              LOCAL SAVED STORAGE                                              
         SPACE 1                                                                
LWSD     DSECT                                                                  
RELO     DS    F                                                                
ATEMP    DS    A                                                                
ALEDGER  DS    A                                                                
         SPACE 1                                                                
FRSTKEY  DS    CL42                                                             
LASTKEY  DS    CL42                                                             
YEAR     DS    CL1                                                              
YTD      DS    PL6                                                              
CSHD     DS    PL6                                                              
PWRK     DS    PL13                                                             
         SPACE 1                                                                
BUDST    DS    CL2                                                              
BUDEN    DS    CL2                                                              
QTRNUM   DS    CL1                                                              
         SPACE 1                                                                
FRMTYPE  DS    CL1                                                              
TOTYPE   DS    CL1                                                              
DSPANY   DS    CL1                 N=NOTHING DISPLAY                            
INVCNT   DS    CL1                 NUMBER IN TABLE                              
INVNXT   DS    CL1                 NUMBER OF THE NEXT ITEM TO DISPLAY           
         SPACE 1                                                                
TRANMODE DS    CL1                 X'00'=DISPLAY                                
*                                  X'01'=TRANSFER                               
KEYCHA   DS    CL1                 ANY KEY CHANGES 'Y' OR 'N'                   
ACN      DS    CL5                                                              
AGYCDE   DS    CL3                                                              
LDGR     DS    CL2                                                              
LSTACN   DS    CL8                                                              
PRD      DS    CL2                                                              
MEDIA    DS    CL2                                                              
PRDNME   DS    CL36                                                             
REVSW    DS    CL1                                                              
TRANSW   DS    CL1                                                              
MYSBR    DS    XL1                                                              
SEAMT    DS    PL6                                                              
SXAMT    DS    PL6                                                              
STDATE   DS    CL3                                                              
ENDATE   DS    CL3                                                              
SVKEY    DS    CL42                                                             
LEDGTAB  DS    XL(LDGTABL+1)                                                    
         SPACE 1                                                                
       ++INCLUDE ACADDTRND                                                      
         SPACE 1                                                                
INVLST   DS    CL(ITLEN*MAXITM)                                                 
         SPACE 1                                                                
LWSLEN   EQU   *-LWSD              LENGTH OF SAVED STORAGE                      
         EJECT ,                                                                
*              DSECT FOR TEMP STORAGE                                           
TEMPD    DSECT                                                                  
SXCON    DS    0D                                                               
         DS    CL2048              SX CONTRA                                    
TEMPEND  EQU   *                                                                
         EJECT ,                                                                
*              DSECT TO COVER SCREEN LINE                                       
         SPACE 1                                                                
LINED    DSECT                                                                  
LINH     DS    CL8                                                              
LINMED   DS    CL2                 MEDIA                                        
         DS    CL1                                                              
LINMTH   DS    CL5                 MONTH                                        
         DS    CL1                                                              
LINVEH   DS    CL26                VEHICLE                                      
         DS    CL1                                                              
LININV   DS    CL6                 INVOICE                                      
         DS    CL1                                                              
LINAMT   DS    CL10                AMOUNT                                       
         DS    CL1                                                              
LINTOT   DS    CL10                TOTAL                                        
         DS    CL1                                                              
LINDLEN  EQU   *-LINMED                                                         
LINTRNH  DS    CL8                                                              
LINTRN   DS    CL8                 TO ACCOUNT                                   
LINLEN   EQU   *-LINED                                                          
         EJECT ,                                                                
*              DSECT TO COVER ENTRY IN INVLST                                   
ITD      DSECT                                                                  
ITMED    DS    CL2                 MEDIA                                        
ITYRM    DS    CL2                 YEAR/MONTH FOR SORT                          
ITQTR    DS    CL1                 QUARTER NUMBER FOR SPOT                      
ITVEH    DS    CL8                 VEHICLE                                      
ITDTE    DS    CL3                 DATE                                         
ITINV    DS    CL6                 INVOICE NUMBER                               
ITSBRSE  DS    CL1                 SUB-REFERENCE ON SE                          
ITSBRSX  DS    CL1                 SUB-REFERENCE ON SX                          
ITACN    DS    CL5                 TO ACN                                       
ITAGY    DS    CL3                 TO AGENCY                                    
ITAMT    DS    PL6                 AMOUNT                                       
ITLEN    EQU   *-ITD                                                            
         EJECT ,                                                                
       ++INCLUDE ACEXPFFD                                                       
         EJECT ,                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE ACEXPF9D                                                       
         EJECT ,                                                                
       ++INCLUDE DDGENTWA                                                       
         EJECT ,                                                                
* ACEXPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACEXPWORKD                                                     
         PRINT ON                                                               
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACLDGTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACLDGTABD                                                      
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030ACEXP06   01/27/04'                                      
         END                                                                    
