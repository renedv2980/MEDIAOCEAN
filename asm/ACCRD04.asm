*          DATA SET ACCRD04    AT LEVEL 006 AS OF 05/01/02                      
*PHASE T60D04A                                                                  
         TITLE 'T60D04 - ALLOW/DISALLOW CASH DISCOUNT.'                         
T60D01   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 000,**DIS**                                                      
         L     RC,0(R1)                                                         
         USING T60DD,RC                                                         
         USING T60DFFD,RA                                                       
         USING CRDSAVD,R8                                                       
         XC    CRDTHED,CRDTHED                                                  
         XC    CRDTOT,CRDTOT                                                    
         OI    CRDTHEDH+6,X'80'                                                 
         OI    CRDTOTH+6,X'80'                                                  
         MVC   CRDHHED+16(6),=C'INV-NO'                                         
         MVC   CRDHHED+56(6),CRDHHED+16                                         
         OI    CRDHHEDH+6,X'80'                                                 
         B     DIS610                                                           
EXIT     CR    RB,RB                                                            
         B     EXIT2                                                            
*                                                                               
ERRXIT   LTR   RB,RB                                                            
*                                                                               
EXIT2    XMOD1 1                                                                
         EJECT                                                                  
*              START OF MAIN LOOP                                               
*                                                                               
DIS610   CLI   MODE,C'M'           IF MODE = M                                  
         BE    DIS800              MARK RECORDS FROM PREVIOUS DISPLAY.          
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
         USING TRCASHD,RE                                                       
         USING KEYTABD,RF                                                       
         LA    RE,SAVE50                                                        
         LA    RF,KEYTAB(RF)                                                    
         MVC   KTBKEY,KEY+L'ACKEYACC                                            
         MVC   KTBDSTAT,TRCSTYPE                                                
         SP    TRANSCT,=P'1'                                                    
         B     EXIT                                                             
         DROP  RE,RF                                                            
         EJECT                                                                  
DIS800   LA    RF,CRDMRKH          FIND FIRST SCREEN ITEM.                      
         USING CRDLYND,RF                                                       
         B     DIS824                                                           
*                                                                               
DIS820   LA    RF,CRLNLNQ(RF)                                                   
DIS824   CP    TRANSCT,=P'1'                                                    
         BL    DIS880                                                           
         ZAP   DUB,=P'28'          GET THIS KEY FROM KEYTABLE                   
         SP    DUB,TRANSCT                                                      
         CVB   R1,DUB                                                           
         LA    R0,KTBLNQ                                                        
         USING KEYTABD,R1                                                       
         MR    R0,R0                                                            
         LA    R1,KEYTAB(R1)                                                    
         OC    KTBKEY,KTBKEY                                                    
         BZ    DIS880              NO MORE SAVED KEYS.                          
         SP    TRANSCT,=P'1'                                                    
*                                                                               
DIS825   CLI   CRLNMRK,C'Y'        IF THE TRANS HAS BEEN MARKED 'Y' OR          
         BE    DIS830              'N', TREAT APPROPRIATELY. IF IT'S            
         CLI   CRLNMRK,C'N'        UNMARKED AND THERE'S NO 'ALL' OPTION         
         BE    DIS826              ON, GET THE NEXT TRANS; IF 'ALL' IS          
         CLI   CRLNMRK,C'D'                                                     
         BE    *+12                                                             
         CLI   CRLNMRKH+5,0                                                     
         BNE   MRKERR                                                           
         CLI   ALL,C' '            ON, SET THE TRANS MARK = ALL AND             
         BE    DIS825B             REEXAMINE THE MARK.                          
         MVC   CRLNMRK,ALL                                                      
         B     DIS825                                                           
*                                                                               
DIS825B  CLI   CRLNMRK,C'D'        IF NO 'ALL' VALUE AND INPUT                  
         BE    DIS840              WAS NOT NULL, THEN EXAMINE STATUS            
         B     DIS820                                                           
*                                                                               
DIS826   CLI   SWITCH,1            MARKED 'N' WITH SWITCH = PAY.                
         BE    DIS840                                                           
         B     DIS844              WITHOUT = UNPAY.                             
DIS830   CLI   SWITCH,1            IF THE SWITCH IS ON,                         
         BE    DIS844              DISAPPROVE MARKED TRANSACTIONS.              
*                                                                               
DIS840   MVI   CRLNMRK,C'D'        MARK AS APPROVED ON SCREEN.                  
         CLI   KTBDSTAT,C'D'                                                    
         BE    DIS862              TRANSACTION ALREADY APPROVED.                
         MVI   KTBCHNG,X'FF'       MARK CHANGED TRANS KEY.                      
         B     DIS860                                                           
*                                                                               
DIS844   MVI   CRLNMRK,C' '        REMOVE MARK FROM SCREEN.                     
         CLI   KTBDSTAT,C'D'                                                    
         BNE   DIS862              NOT APPROVED YET.                            
         MVI   KTBCHNG,X'FF'       MARK CHANGED TRANS KEY.                      
         DROP  R1                                                               
*                                                                               
DIS860   MVI   CHANGE,C'Y'         REMEMBER THE CHANGE.                         
DIS862   OI    CRLNMRKH+6,X'80'                                                 
         B     DIS820                                                           
*                                                                               
DIS880   CLI   CHANGE,C'Y'                                                      
         BNE   DIS900              NO CHANGES, TEST FOR ACC'T END.              
*                                                                               
         USING KEYTABD,R2                                                       
         LA    R2,KEYTAB           SEARCH LIST OF DISPLAYED KEYS FOR            
         LA    R3,28               NON-ZERO ENTRIES.                            
DIS884   OC    KTBKEY,KTBKEY                                                    
         BZ    DIS900                                                           
         B     DIS886                                                           
DIS885   LA    R2,KTBLNQ(R2)                                                    
         BCT   R3,DIS884                                                        
         B     DIS900                                                           
*                                                                               
DIS886   MVC   KEY(3),COMPSAVE                                                  
         MVC   KEY+3(12),ACCSAVE                                                
         MVC   KEY+L'ACKEYACC(L'KTBKEY),KTBKEY                                  
         GOTO1 AIORTNS,PARM,AREAD  GET THE RECORD                               
         LA    RF,IOAREA                                                        
         USING TRCASHD,RF                                                       
DIS886A  CLI   TRCSEL,0                                                         
         BE    DIS890                                                           
         CLI   TRCSEL,X'50'                                                     
         BE    DIS886E                                                          
         ZIC   RE,TRCSLEN                                                       
         AR    RF,RE                                                            
         B     DIS886A                                                          
*                                                                               
DIS886E  CLC   TRCSTYPE,KTBDSTAT                                                
         BNE   DIS890                                                           
         CLI   KTBCHNG,X'FF'                                                    
         BNE   DIS885                                                           
         CLI   TRCSTYPE,C'D'                                                    
         BNE   *+12                                                             
         MVI   TRCSTYPE,C'X'                                                    
         B     *+8                                                              
         MVI   TRCSTYPE,C'D'                                                    
         MVC   KTBDSTAT,TRCSTYPE                                                
         MVI   KTBCHNG,0           RESET THE CHANGE MARKER.                     
         GOTO1 AIORTNS,PARM,AWRITE PUT THE RECORD BACK.                         
         B     DIS885              TRY THE NEXT ONE.                            
         DROP  R2                                                               
DIS890   MVI   ERRNUM,DUBLUPDT     SET ERROR NUMBER AND CURS FIELD.             
         LA    RF,CRDLEGH                                                       
         ST    RF,FADR                                                          
         B     ERRXIT                                                           
*                                                                               
DIS900   B     EXIT                                                             
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
         MVC   CRLNCON,ACKEYCON+1                                               
         MVC   CRLNREF,ACKEYREF                                                 
         GOTO1 DATCON,DMCB,(1,TRNSDATE),(7,CRLNDATE)                            
         LA    R3,SAVE50                                                        
         USING TRCASHD,R3                                                       
         EDIT  (P6,TRCSAMNT),(10,CRLNAMNT),2,FLOAT=-                            
         CLI   TRCSTYPE,C'D'                                                    
         BNE   *+8                                                              
         MVI   CRLNMRK,C'D'                                                     
         OI    CRLNMRKH+6,X'80'                                                 
         OI    CRLNDISH+6,X'80'                                                 
         B     EXIT                                                             
         DROP  R3,RF                                                            
*                                                                               
MRKERR   MVI   ERRNUM,INVALID                                                   
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
**PAN#1  DC    CL21'006ACCRD04   05/01/02'                                      
         END                                                                    
