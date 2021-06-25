*          DATA SET ACCRD01    AT LEVEL 006 AS OF 05/01/02                      
*PHASE T60D01A                                                                  
         TITLE 'T60D01 - APPROVE CHECKS FOR PAYING.'                            
T60D01   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 000,**APP**                                                      
         L     RC,0(R1)                                                         
         USING T60DD,RC                                                         
         USING T60DFFD,RA                                                       
         USING CRDSAVD,R8                                                       
         MVC   CRDTHED+36(8),=C'APPROVED'                                       
         OI    CRDTHEDH+6,X'80'                                                 
         MVC   CRDHHED+16(6),=C'INV-NO'                                         
         MVC   CRDHHED+56(6),CRDHHED+16                                         
         OI    CRDHHEDH+6,X'80'                                                 
         B     APP610                                                           
EXIT     CR    RB,RB                                                            
         B     EXIT2                                                            
*                                                                               
ERRXIT   LTR   RB,RB                                                            
*                                                                               
EXIT2    XMOD1 1                                                                
         EJECT                                                                  
*              START OF MAIN LOOP                                               
*                                                                               
APP610   CLI   MODE,C'M'           IF MODE = M                                  
         BE    APP800              MARK RECORDS FROM PREVIOUS DISPLAY.          
         CLI   ACCEND,C'Y'          NO SPECIAL CODE FOR ACCOUNT END.            
         BE    EXIT                                                             
         CP    TRANSCT,=P'0'                                                    
         BE    EXIT                NOR FOR SCREEN END.                          
*                                                                               
*              READ AND DISPLAY TRANSACTIONS.                                   
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
         SP    TRANSCT,=P'1'                                                    
         B     EXIT                                                             
         DROP  RE,RF                                                            
         EJECT                                                                  
APP800   LA    RF,CRDMRKH          FIND FIRST SCREEN ITEM.                      
         USING CRDLYND,RF                                                       
         B     APP824                                                           
*                                                                               
APP820   LA    RF,CRLNLNQ(RF)                                                   
APP824   CP    TRANSCT,=P'1'                                                    
         BL    EXIT                                                             
         ZAP   DUB,=P'28'          GET THIS KEY FROM KEYTABLE                   
         SP    DUB,TRANSCT                                                      
         CVB   R1,DUB                                                           
         LA    R0,KTBLNQ                                                        
         USING KEYTABD,R1                                                       
         MR    R0,R0                                                            
         LA    R1,KEYTAB(R1)                                                    
         OC    KTBKEY,KTBKEY                                                    
         BZ    EXIT                NO MORE SAVED KEYS.                          
         SP    TRANSCT,=P'1'                                                    
*                                                                               
APP825   CLI   CRLNMRK,C'Y'        IF THE TRANS HAS BEEN MARKED 'Y' OR          
         BE    APP830              'N', TREAT APPROPRIATELY. IF IT'S            
         CLI   CRLNMRK,C'N'        UNMARKED AND THERE'S NO 'ALL' OPTION         
         BE    APP826              ON, GET THE NEXT TRANS; IF 'ALL' IS          
         CLI   CRLNMRK,C'P'                                                     
         BE    APP825A                                                          
         CLI   CRLNMRKH+5,0                                                     
         BE    APP825A                                                          
         CLI   CRLNMRK,C' '                                                     
         BNE   MRKERR              INVALID INPUT.                               
APP825A  CLI   ALL,C' '            ON, SET THE TRANS MARK = ALL AND             
         BE    APP825B             REEXAMINE THE MARK.                          
         MVC   CRLNMRK,ALL                                                      
         B     APP825                                                           
*                                                                               
APP825B  CLI   CRLNMRK,C'P'        IF NO 'ALL' VALUE AND INPUT                  
         BE    APP840              WAS NOT NULL, THEN EXAMINE STATUS            
         CLI   CRLNMRK,C'N'                                                     
         BE    APP844                                                           
         B     APP862                                                           
*                                                                               
APP826   CLI   SWITCH,1            MARKED 'N' WITH SWITCH = PAY.                
         BE    APP840                                                           
         B     APP844              WITHOUT = UNPAY.                             
APP830   CLI   SWITCH,1            IF THE SWITCH IS ON,                         
         BE    APP844              DISAPPROVE MARKED TRANSACTIONS.              
*                                                                               
APP840   MVI   CRLNMRK,C'P'        MARK AS APPROVED ON SCREEN.                  
         TM    KTBDSTAT,X'02'                                                   
         BO    APP862              TRANSACTION ALREADY APPROVED.                
         MVI   KTBCHNG,X'FF'       MARK CHANGED TRANS KEY.                      
         B     APP860                                                           
*                                                                               
APP844   MVI   CRLNMRK,C' '        REMOVE MARK FROM SCREEN.                     
         TM    KTBDSTAT,X'02'                                                   
         BZ    APP862              NOT APPROVED YET.                            
         MVI   KTBCHNG,X'FF'       MARK CHANGED TRANS KEY.                      
*                                                                               
APP860   MVI   CHANGE,C'Y'         REMEMBER THE CHANGE.                         
APP862   OI    CRLNMRKH+6,X'80'                                                 
         CLI   SYSTPRF2,C'Y'       IF INVOICE AUTHORIZATION SYSTEM              
         BNE   APP820              IS IN USE AND IF THIS IS A                   
         CLC   ULSAVE,=C'SV'       PRODUCTION SUPPLIER                          
         BNE   APP820                                                           
         TM    KTBDSTAT,X'08'      MARK UNAUTHORIZED INVOICES                   
         BO    APP820                                                           
         MVI   CRLNAUTH,C'*'                                                    
         CLI   KTBCHNG,X'FF'       CANNOT CHANGE UNAUTHORIZED ITEMS             
         BNE   APP820              HE DIDN'T                                    
         MVI   KTBCHNG,0           ERASE THE CHANGE MARK                        
         B     AUTHERR                                                          
         DROP  R1                                                               
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
         TM    TRNSSTAT,X'02'                                                   
         BZ    *+8                                                              
         MVI   CRLNMRK,C'P'                                                     
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
         TM    TRNSSTAT,X'02'                                                   
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
**PAN#1  DC    CL21'006ACCRD01   05/01/02'                                      
         END                                                                    
