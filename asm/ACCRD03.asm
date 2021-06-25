*          DATA SET ACCRD03    AT LEVEL 008 AS OF 05/01/02                      
*PHASE T60D03A                                                                  
         TITLE 'T60D03 - REVERSAL IDENTIFICATION OVERLAY.'                      
T60D03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 000,**RVRS**                                                     
         L     RC,0(R1)                                                         
         USING T60DD,RC                                                         
         USING T60DFFD,RA                                                       
         USING CRDSAVD,R8                                                       
         MVC   CRDTHED+36(8),=C'REVERSED'                                       
         OI    CRDTHEDH+6,X'80'                                                 
         MVC   CRDHHED+16(6),=C'INV-NO'                                         
         MVC   CRDHHED+56(6),CRDHHED+16                                         
         OI    CRDHHEDH+6,X'80'                                                 
         B     RVS610                                                           
EXIT     CR    RB,RB                                                            
         B     EXIT2                                                            
*                                                                               
ERRXIT   LTR   RB,RB                                                            
*                                                                               
EXIT2    XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
RVS610   CLI   MODE,C'M'           IF MODE = M                                  
         BE    RVS800              MARK RECORDS FROM PREVIOUS DISPLAY.          
         CP    TRANSCT,=P'0'                                                    
         BE    EXIT                SCREEN IS FULL.                              
         CLI   ACCEND,C'Y'                                                      
         BE    EXIT                ACCOUNT END.                                 
*                                                                               
*              SAVE AND DISPLAY TRANSACTIONS.                                   
*                                                                               
         BAS   RE,DISPLAY                                                       
         ZAP   DUB,=P'28'          FIND SPACE IN TABLE OF KEYS TO               
         SP    DUB,TRANSCT         BE MARKED AND INSERT THIS KEY.               
         CVB   RF,DUB                                                           
         LA    RE,L'KEYTAB                                                      
         MR    RE,RE                                                            
         USING TRANSD,RE                                                        
         USING KEYTABD,RF                                                       
         LA    RE,IOAREA                                                        
         LA    RF,KEYTAB(RF)                                                    
         MVC   KTBKEY,KEY+L'ACKEYACC                                            
         MVC   KTBDSTAT,TRNSSTAT                                                
         ZAP   KTBAMNT,TRNSAMNT                                                 
         SP    TRANSCT,=P'1'                                                    
         B     EXIT                                                             
         DROP  RE,RF                                                            
         EJECT                                                                  
RVS800   LA    RF,CRDMRKH          FIND FIRST SCREEN ITEM.                      
         USING CRDLYND,RF                                                       
         ZAP   DOUBLE,=P'0'                                                     
         B     RVS824                                                           
*                                                                               
RVS820   LA    RF,CRLNLNQ(RF)                                                   
RVS824   CP    TRANSCT,=P'1'                                                    
         BL    RVS880                                                           
         ZAP   DUB,=P'28'          GET THIS KEY FROM KEYTABLE                   
         SP    DUB,TRANSCT                                                      
         CVB   R1,DUB                                                           
         LA    R0,KTBLNQ                                                        
         USING KEYTABD,R1                                                       
         MR    R0,R0                                                            
         LA    R1,KEYTAB(R1)                                                    
         OC    KTBKEY,KTBKEY                                                    
         BZ    RVS880              NO MORE SAVED KEYS.                          
         SP    TRANSCT,=P'1'                                                    
*                                                                               
RVS825   CLI   CRLNMRK,C'Y'        IF ITEM IS MARKED Y OR N, HANDLE             
         BE    RVS830              NORMALLY.                                    
         CLI   CRLNMRK,C'N'                                                     
         BE    RVS826                                                           
         CLI   CRLNMRK,REVCH                                                    
         BE    RVS825A                                                          
         CLI   CRLNMRKH+5,0                                                     
         BE    RVS825A                                                          
         CLI   CRLNMRK,C' '                                                     
         BNE   MRKERR                                                           
RVS825A  CLI   ALL,C' '            MARK ITEMS WITH CONTENTS OF ALL.             
         BE    RVS825B                                                          
RVS8252  MVC   CRLNMRK,ALL                                                      
         B     RVS825                                                           
*                                                                               
RVS825B  CLI   CRLNMRK,REVCH       IF NO 'ALL' VALUE AND INPUT                  
         BE    RVS840              WAS NOT NULL, THEN EXAMINE STATUS            
         B     RVS862                                                           
*                                                                               
RVS826   CLI   SWITCH,1            MARKED 'N' WITH SWITCH = PAY.                
         BE    RVS840                                                           
         B     RVS844              WITHOUT = UNPAY.                             
RVS830   CLI   SWITCH,1            IF THE SWITCH IS ON,                         
         BE    RVS844              DISAPPROVE MARKED TRANSACTIONS.              
*                                                                               
RVS840   MVI   CRLNMRK,REVCH       MARK AS APPROVED ON SCREEN.                  
         AP    DOUBLE,KTBAMNT      TO BE MARKED ARE SUMMED ALSO.                
         TM    KTBDSTAT,X'20'                                                   
         BO    RVS862              TRANSACTION ALREADY APPROVED.                
         MVI   KTBCHNG,X'FF'       MARK CHANGED TRANS KEY.                      
         B     RVS860                                                           
*                                                                               
RVS844   MVI   CRLNMRK,C' '        REMOVE MARK FROM SCREEN.                     
         AP    DOUBLE,KTBAMNT      SUM UNMARKINGS.                              
         TM    KTBDSTAT,X'20'                                                   
         BZ    RVS862              NOT APPROVED YET.                            
         MVI   KTBCHNG,X'FF'       MARK CHANGED TRANS KEY.                      
*                                                                               
RVS860   MVI   CHANGE,C'Y'         REMEMBER THE CHANGE.                         
RVS862   OI    CRLNMRKH+6,X'80'                                                 
         CLI   SYSTPRF2,C'Y'       MARK UNAUTHORIZED TRANSACTIONS               
         BNE   RVS820              ON PRODUCTION SUPPLIERS FOR                  
         CLC   ULSAVE,=C'SV'       AGENCIES ON INVOICE AUTHOR-                  
         BNE   RVS820              IZATION SYSTEM                               
         TM    KTBDSTAT,X'08'                                                   
         BO    RVS820                                                           
         MVI   CRLNAUTH,C'*'                                                    
         B     RVS820                                                           
*                                                                               
RVS880   CP    DOUBLE,=P'0'                                                     
         BE    EXIT                REVERSED ITEMS BALANCE, GOOD EXIT.           
         LA    R1,KEYTAB           ERASE TABLE ENTRY CHANGE FLAGS               
RVS900   OC    KTBKEY,KTBKEY                                                    
         BZ    RVS920              END OF TABLE                                 
         MVI   KTBCHNG,0                                                        
         LA    R1,KTBLNQ(R1)                                                    
         B     RVS900                                                           
         DROP  R1                                                               
*                                                                               
RVS920   MVI   ERRNUM,REVIMBAL                                                  
         LA    RF,CRDMRKH                                                       
         ST    RF,FADR                                                          
         LTR   RB,RB               GIVE CC NEQ FOR ERROR EXIT                   
         B     ERRXIT                                                           
         EJECT                                                                  
*                                                                               
*              DISPLAY TRANSACTIONS                                             
*                                                                               
DISPLAY  NTR1                                                                   
         LA    R3,IOAREA                                                        
         LA    RF,KEY                                                           
         USING TRANSD,R3                                                        
         USING ACKEYD,RF                                                        
         ZAP   DUB,=P'28'          FIND NEXT SLOT FOR TRANSACTION               
         SP    DUB,TRANSCT         DETAILS.                                     
         CVB   R1,DUB                                                           
         LA    R0,CRLNLNQ                                                       
         MR    R0,R0                                                            
         LA    R2,CRDMRKH(R1)                                                   
         USING CRDLYND,R2                                                       
         MVC   CRLNCON(35),SPACES                                               
*                                                                               
         MVI   CRLNMRK,C' '                                                     
         TM    TRNSSTAT,X'20'                                                   
         BZ    *+8                                                              
         MVI   CRLNMRK,REVCH                                                    
         CLC   ULSAVE,=C'SV'                                                    
         BNE   DIS040                                                           
         CLI   SYSTPRF2,C'Y'                                                    
         BNE   *+16                NOT USING 08 BIT                             
         TM    TRNSSTAT,X'08'                                                   
         BO    *+8                 AUTHORIZED                                   
         MVI   CRLNAUTH,C'*'                                                    
DIS040   MVC   CRLNCON,ACKEYCON+1                                               
         MVC   CRLNREF,ACKEYREF                                                 
         GOTO1 DATCON,DMCB,(1,TRNSDATE),(7,CRLNDATE)                            
         EDIT  (P6,TRNSAMNT),(10,CRLNAMNT),2,FLOAT=-                            
         OI    CRLNMRKH+6,X'80'                                                 
         OI    CRLNDISH+6,X'80'                                                 
         AP    ACCTOT,TRNSAMNT                                                  
         TM    TRNSSTAT,X'20'                                                   
         BO    *+14                                                             
         AP    ACCBAL,TRNSAMNT                                                  
         B     *+10                                                             
         AP    ACCMRKD,TRNSAMNT                                                 
         B     EXIT                                                             
         DROP  R3,RF                                                            
*                                                                               
MRKERR   MVI   ERRNUM,INVALID                                                   
         ST    RF,FADR                                                          
         B     ERRXIT                                                           
*                                                                               
AUTHERR  MVI   ERRNUM,AUTHMRK                                                   
         ST    RF,FADR                                                          
         B     ERRXIT                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*&&US                                                                           
REVCH    EQU   C'R'                                                             
*&&                                                                             
*&&UK                                                                           
REVCH    EQU   C'M'                                                             
*&&                                                                             
T60DFFD  DSECT                                                                  
         DS    CL16                                                             
MYCOMPEL DS    CL40                                                             
         DS    CL8                                                              
       ++INCLUDE ACCRDFFD                                                       
OLDKEY   DS    CL42                                                             
COMPSAVE DS    CL1                                                              
ULSAVE   DS    CL2                                                              
         EJECT                                                                  
*        FATWA                                                                  
*        ACGENBOTH                                                              
*        DDFLDIND                                                               
*        DDCOMFACS                                                              
*        DDACCFACS                                                              
         PRINT OFF                                                              
       ++INCLUDE FATWA                                                          
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDACCFACS                                                      
         PRINT ON                                                               
*                                                                               
*                                                                               
       ++INCLUDE ACCRDSECT                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACCRD03   05/01/02'                                      
         END                                                                    
