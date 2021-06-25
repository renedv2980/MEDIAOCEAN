*          DATA SET ACCRD02    AT LEVEL 013 AS OF 05/01/02                      
*PHASE T60D02A                                                                  
         TITLE 'T60D02 - BANK RECONCILIATION OVERLAY.'                          
T60D02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 000,**RECN**                                                     
         L     RC,0(R1)                                                         
         USING T60DD,RC                                                         
         USING T60DFFD,RA                                                       
         USING CRDSAVD,R8                                                       
         MVC   CRDTHED+36(8),=C' CLEARED'                                       
         OI    CRDTHEDH+6,X'80'                                                 
*&&US*&& MVC   CRDHHED+16(6),=C'CHECK '                                         
*&&UK*&& MVC   CRDHHED+16(6),=C'CHEQUE'                                         
         MVC   CRDHHED+56(6),CRDHHED+16                                         
         OI    CRDHHEDH+6,X'80'                                                 
         B     RCN610                                                           
*                                                                               
EXIT     CR    RB,RB                                                            
         B     EXIT2                                                            
*                                                                               
ERRXIT   LTR   RB,RB                                                            
*                                                                               
EXIT2    XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
* RECCHKEY = LO KEY FOR THIS PASS: RECNXKEY = HI KEY OF THIS PASS               
* NEXTKEY = KEY OF CURRENT TRANSACTION.                                         
*                                                                               
RCN610   CLI   MODE,C'M'           IF MODE = M                                  
         BE    RCN800              LOOK FOR MARKS.                              
         CLI   ACCEND,C'Y'         DISPLAY ITEMS AT ACCOUNT END.                
         BNE   RCN700              ELSE JUST SLOT INTO TABLE.                   
         CP    TRANSCT,=P'28'                                                   
         BE    EXIT                NO ITEMS IN THIS PASS.                       
         CLI   TABCNT,0                                                         
         BNE   *+14                ITEMS THERE, DISPLAY THEM.                   
         ZAP   TRANSCT,=P'28'      NONE, RETURN TO ROOT.                        
         B     EXIT                                                             
RCN620   BAS   RE,DISPLAY          DISPLAY THIS LOAD                            
         CLI   TABCNT,28           LESS THAN A FULL SCREEN                      
         BNE   EXIT                MEANS IT REALLY IS THE END                   
         MVC   RECCHKEY,RECNXKEY   SET LOW KEY FOR NEXT PASS                    
         MVC   OLDKEY(15),KEYSAVE  RESET BANK ACCOUNT                           
         MVI   OLDKEY+15,C' '                                                   
         MVC   OLDKEY+16(26),OLDKEY+15                                          
         MVI   ACCEND,C'N'         PRETEND NOT ACCOUNT END                      
         CR    RB,RB               FORCE CC EQU.                                
         B     EXIT                                                             
*                                                                               
RCN700   CP    TRANSCT,=P'28'      FOR 1ST ITEM IN PASS,                        
         BNE   RCN710                                                           
         MVI   TABCNT,0            RESET TABLE COUNTER.                         
         SP    TRANSCT,=P'1'                                                    
         OC    RECNXKEY,RECNXKEY   ANYTHING IN HIGH KEY                         
         BNZ   RCN707              NOT PASS 1.                                  
         XC    RECCHKEY,RECCHKEY                                                
         MVC   RECCHKEY(6),FRSTCHEK  SET LO KEY FROM FILTER                     
         ZIC   R1,RECCHKEY+5       REDUCE LO KEY BY 1                           
         BCTR  R1,0                FOR BNH BELOW                                
         STC   R1,RECCHKEY+5                                                    
RCN707   XC    RECNXKEY,RECNXKEY   HI KEY THIS PASS                             
*                                                                               
         USING ACKEYD,RF                                                        
RCN710   LA    RF,KEY                                                           
         MVC   NEXTKEY(7),ACKEYREF                                              
         MVC   NEXTKEY+7(20),ACKEYWRK                                           
         CLC   NEXTKEY(27),RECCHKEY                                             
         BNH   EXIT                CHECK NO. NGT LOWEST REQUIRED                
RCN714   CLI   TABCNT,28           CHECK IF TABLE IS FULL.                      
         BL    RCN720              NOT FULL, NORMAL INSERTION.                  
         CLC   NEXTKEY(27),RECNXKEY    IF CHECK NO IS GT HIEST                  
         BNL   EXIT                SO FAR,REJECT IT.                            
         MVI   TABCNT,27           DISPLACE THE HIEST AND INSERT THIS.          
         USING KEYTABD,R2                                                       
*                                                                               
RCN720   LA    R2,TABREC                                                        
         MVC   KTBKEY,ACKEYWRK     HOLD KEY LESS C/U/L/ACC'T/.                  
         LA    R3,IOAREA                                                        
         USING TRANSD,R3                                                        
         MVC   KTBDSTAT,TRNSSTAT                                                
         GOTO1 DATCON,PARM,(1,TRNSDATE),(2,KTBDATE)                             
         MVC   KTBAMNT,TRNSAMNT                                                 
         DROP  R2,R3                                                            
         BAS   RE,INSERT           INSERT THE RECORD IN THE TABLE.              
         LA    R3,KTBLNQ                                                        
         ZIC   R2,TABCNT                                                        
         BCTR  R2,0                                                             
         MR    R2,R2                                                            
         LA    R2,KEYTAB(R3)       POINT TO HIGHEST CHEK THIS PASS              
         MVC   RECNXKEY(7),20(R2)     AND RESET HIKEY                           
         MVC   RECNXKEY+7(20),0(R2)                                             
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
RCN800   LA    RF,CRDMRKH          FIND FIRST SCREEN ITEM.                      
         USING CRDLYND,RF                                                       
         B     RCN824                                                           
*                                                                               
RCN820   LA    RF,CRLNLNQ(RF)                                                   
RCN824   CP    TRANSCT,=P'1'                                                    
         BL    RCN880                                                           
         ZAP   DUB,=P'28'          GET THIS KEY FROM KEYTABLE                   
         SP    DUB,TRANSCT                                                      
         CVB   R1,DUB                                                           
         LA    R0,KTBLNQ                                                        
         USING KEYTABD,R1                                                       
         MR    R0,R0                                                            
         LA    R1,KEYTAB(R1)                                                    
         OC    KTBKEY,KTBKEY                                                    
         BZ    RCN880              NO MORE SAVED KEYS.                          
         SP    TRANSCT,=P'1'                                                    
*                                                                               
RCN825   CLI   CRLNMRK,C'Y'        IF THE TRANS HAS BEEN MARKED 'Y' OR          
         BE    RCN830              'N', TREAT APPROPRIATELY. IF IT'S            
         CLI   CRLNMRK,C'N'        UNMARKED AND THERE'S NO 'ALL' OPTION         
         BE    RCN826              ON, GET THE NEXT TRANS; IF 'ALL' IS          
         CLI   CRLNMRK,C'C'                                                     
         BE    *+12                                                             
         CLI   CRLNMRKH+5,0                                                     
         BNE   MRKERR                                                           
         CLI   ALL,C' '            ON, SET THE TRANS MARK = ALL AND             
         BE    RCN825A             REEXAMINE THE MARK.                          
         MVC   CRLNMRK,ALL                                                      
         B     RCN825                                                           
*                                                                               
RCN825A  CLI   CRLNMRK,C'C'        IF NO 'ALL' VALUE AND INPUT                  
         BE    RCN840              WAS NOT NULL, THEN EXAMINE STATUS            
         B     RCN862                                                           
*                                                                               
RCN826   CLI   SWITCH,1            MARKED 'N' WITH SWITCH = PAY.                
         BE    RCN840                                                           
         B     RCN844              WITHOUT = UNPAY.                             
RCN830   CLI   SWITCH,1            IF THE SWITCH IS ON,                         
         BE    RCN844              DISAPPROVE MARKED TRANSACTIONS.              
*                                                                               
RCN840   MVI   CRLNMRK,C'C'        MARK AS APPROVED ON SCREEN.                  
         TM    KTBDSTAT,X'02'                                                   
         BO    RCN862              TRANSACTION ALREADY APPROVED.                
         MVI   KTBCHNG,X'FF'       MARK CHANGED TRANS KEY.                      
         B     RCN860                                                           
*                                                                               
RCN844   MVI   CRLNMRK,C' '        REMOVE MARK FROM SCREEN.                     
         TM    KTBDSTAT,X'02'                                                   
         BZ    RCN862              NOT APPROVED YET.                            
         MVI   KTBCHNG,X'FF'       MARK CHANGED TRANS KEY.                      
         DROP  R1                                                               
*                                                                               
RCN860   MVI   CHANGE,C'Y'         REMEMBER THE CHANGE.                         
RCN862   OI    CRLNMRKH+6,X'80'                                                 
         B     RCN820                                                           
*                                                                               
RCN880   B     EXIT                EXIT WITH CC OF = FOR O.K.                   
         DROP  RF                                                               
         EJECT                                                                  
*                                                                               
*              DISPLAY TRANSACTIONS                                             
*                                                                               
DISPLAY  NTR1                                                                   
         ZIC   R2,TABCNT           R2 = NO OF ITEMS TO BE DISPLAYED.            
         LA    R4,KEYTAB           R4 = A(ITEMS).                               
         USING KEYTABD,R4                                                       
         LA    R3,CRDMRKH          R3 = A(1ST DISPLAY POSITION).                
         USING CRDLYND,R3                                                       
*                                                                               
DIS020   TM    KTBDSTAT,X'02'                                                   
         BZ    *+8                                                              
         MVI   CRLNMRK,C'C'        ITEM HAS CLEARED PREVIOUSLY.                 
         MVC   CRLNCON(11),KTBKEY+3 CONTRA-ACC'T LESS CO. CODE                  
         MVC   CRLNREF,KTBKEY+20   CHECK NUMBER.                                
         EDIT  (P6,KTBAMNT),(10,CRLNAMNT),2,FLOAT=-                             
         GOTO1 DATCON,PARM,(2,KTBDATE),(7,CRLNDATE)                             
         OI    CRLNMRKH+6,X'80'                                                 
         OI    CRLNDISH+6,X'80'                                                 
         AP    ACCTOT,KTBAMNT                                                   
         TM    KTBDSTAT,X'02'                                                   
         BO    *+14                                                             
         AP    ACCBAL,KTBAMNT                                                   
         B     *+10                                                             
         AP    ACCMRKD,KTBAMNT                                                  
         LA    R3,CRLNLNQ(R3)      NEXT DISPLAY POSITION.                       
         LA    R4,KTBLNQ(R4)       NEXT ITEM TO BE DISPLAYED.                   
         BCT   R2,DIS020                                                        
         B     EXIT                                                             
         DROP  R3,R4                                                            
*                                                                               
*                                                                               
INSERT   NTR1                                                                   
         ZIC   RF,TABCNT           RF = NO OF ITEMS IN TABLE.                   
         LR    R1,RF               R1 = RF.                                     
         BCTR  RF,0                                                             
         LA    RE,KTBLNQ                                                        
         MR    RE,RE                                                            
         LA    RF,KEYTAB(RF)       RF = A(1ST FREE SLOT-L'SLOT).                
         LTR   R1,R1                                                            
         BZ    INS040              FIRST ITEM, DON'T SEARCH.                    
         USING KEYTABD,RF                                                       
*                                                                               
INS020   CLC   TABREC+20(7),KTBKEY+20   COMPARE CHECK NUMBERS.                  
         BNL   INS040              HI, INSERT BEHIND THIS ONE.                  
         MVC   KTBLNQ(KTBLNQ,RF),0(RF)  SHIFT THE TABLE ENTR BACKWARDS.         
         LA    RE,KTBLNQ                                                        
         SR    RF,RE               BUMP DOWN TABLE.                             
         BCT   R1,INS020           TRY AGAIN.                                   
*                                                                               
INS040   MVC   KTBLNQ(KTBLNQ,RF),TABREC INSERT THIS ITEM.                       
         ZIC   RF,TABCNT                COUNT IT.                               
         LA    RF,1(RF)                                                         
         STC   RF,TABCNT                                                        
         B     EXIT                                                             
         DROP  RF                                                               
*                                                                               
MRKERR   MVI   ERRNUM,INVALID                                                   
         ST    RF,FADR                                                          
         B     ERRXIT                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
OVERDUPE EQU   93                                                               
T60DFFD  DSECT                                                                  
         DS    CL16                                                             
MYCOMPEL DS    CL40                                                             
         DS    CL8                 SPARE.                                       
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
**PAN#1  DC    CL21'013ACCRD02   05/01/02'                                      
         END                                                                    
