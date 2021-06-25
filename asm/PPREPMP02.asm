*          DATA SET PPREPMP02  AT LEVEL 045 AS OF 01/12/09                      
*PHASE PPMP02A                                                                  
*INCLUDE MININAM                                                                
         TITLE 'PPMP02 - PRTFIX PROGRAM'                                        
*                                                                               
*   THIS PROGRAM WILL SEARCH FOR THE ONE BYTE INSERTION YEAR WHICH              
*   PROVIDED BY QOPT6 OF THE REQUEST CARD. IT WILL FLAG ALL MATCHING            
*   RECORD(S) WITH DELETION.                                                    
*                                                                               
*      QOPT2     Y MEANS DUMP BEFORE AND AFTER                                  
*      QOPT3     Y MEANS RUN FOR ONE ONLY AGY/M/CLT/PRD/PUB                     
*      QOPT5     Y MEANS TEST RUN (DON'T MARK FILE)                             
*      QOPT6     ONE CHARACTER BYTE FOR INSERTION YEAR                          
*                                                                               
PPMP02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPMP02                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP02WRKD,R8                                                      
*                                                                               
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   RECCNT,=P'0'        RECS READ                                    
         ZAP   DELCNT,=P'0'        RECS MARKED FOR DELETION                     
         ZAP   WRTCNT,=P'0'        RECS ACTUALLY WRITTEN                        
         ZAP   INVCNT,=P'0'        INVOICE REC COUNT                            
         XC    AGYTAB,AGYTAB       AGENCIES TO BE PROCESSED                     
         MVI   AGYTABX,X'FF'       END OF AGENCY TABLE                          
         MVI   AGYFLAG,0           AGYTAB FLAG FOR TAB IS FULL                  
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                  FIRST BUY FOR REQUEST                        
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
*                                                                               
         CLI   QOPT6,C'0'          QOPT6= YEAR TO BE DELETED                    
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLI   QOPT6,C'9'                                                       
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY       START AT AGENCY/MEDIA                       
         MVI   KEY+3,X'50'          INVOICE MATCHING RECORD                     
         CLI   QOPT3,C'Y'          MEANS ONLY ONE AGY/MED/CLT/PRD/ETC           
         BNE   PROC2                                                            
         MVC   KEY+04(3),QCLIENT         CLIENT CODE                            
         MVC   KEY+07(3),QPRODUCT        CLIENT CODE                            
*                                                                               
         XC    SVPUB,SVPUB                                                      
         CLI   QPUB,C'0'           ADD PUB IF NECESSARY                         
         BL    PROC2                                                            
         MVC   WORK(11),QPUB                                                    
         LA    R2,15                                                            
         CLC   QPUB+8(3),=C'ZZZ'   SEE IF DOING ALL ZONES + EDTS                
         BNE   PROC1G                                                           
         LA    R2,13                                                            
         MVC   WORK+8(3),SPACES    USE 'BASE' PUB NUMBER                        
PROC1G   GOTO1 PUBVAL,DMCB,WORK,SVPUB                                           
         MVC   KEY+10(6),SVPUB                                                  
*                                                                               
PROC2    GOTO1 HIGH                                                             
         B     PROC5                                                            
*                                                                               
PROC3    GOTO1 SEQ                                                              
*                                                                               
PROC5    DS    0H                                                               
         CLI   QOPT3,C'Y'          MEANS ONLY ONE AGY/MED/CLT/PRD/ETC           
         BNE   PROC5Q                                                           
         CLI   QPUB,C' '           ADD PUB IF NECESSARY                         
         BNH   PROC5G                                                           
         CLC   KEY(16),KEYSAVE     CHECK AGY/MED/RC/CLT/PRD/PUB                 
         BNE   PROC5L              DONE                                         
PROC5G   CLC   KEY(10),KEYSAVE     CHECK AGY/MED/RC/CLT/PRD                     
         BE    PROC5Q                                                           
PROC5L   MVI   KEY,X'FF'                                                        
         MVI   MODE,RUNLAST                                                     
         B     EXIT                                                             
PROC5Q   CLC   KEY(4),KEYSAVE      JUST CHECK AGY/MEDIA RECORD CODE             
         BNE   PROC80              END OF MEDIA GO DO NEXT                      
*                                                                               
         AP    RECCNT,=P'1'                                                     
*                                                                               
         LA    R2,AGYTAB                                                        
PROC45   CLI   0(R2),X'FF'         END OF AGENCY TABLE?                         
         BNE   PROC46                                                           
         MVI   AGYFLAG,C'Y'        AGYTAB IS FULL                               
         B     PROC50                                                           
PROC46   CLI   0(R2),0             EMPTY ENTRY?                                 
         BE    PROC48                                                           
         CLC   0(2,R2),KEY         ALREADY IN TABLE?                            
         BE    PROC50                                                           
         LA    R2,2(R2)            MOVE TO NEXT ENTRY                           
         B     PROC45                                                           
PROC48   MVC   0(2,R2),KEY         PUT AGENCY INTO TABLE                        
*                                                                               
PROC50   LA    R2,KEY                                                           
         USING PINVKEYD,R2                                                      
         CLC   PINVYS,QOPT6        MATCHING INSERTION YEAR BYTE                 
         BNE   PROC3               NO GOOD, READ NEXT RECORD                    
         DROP  R2                                                               
*                                                                               
         GOTO1 GETPRT                                                           
*                                                                               
         LA    R2,PBUYREC+33       SEE IF FIRST ELEM IS INV HDR ELEM            
         CLI   0(R2),X'10'                                                      
         BE    PROC61                                                           
PROC60   MVI   ELCODE,X'10'        INVOICE HEADER ELEM                          
         BAS   RE,NEXTEL                                                        
         BNE   PROC70                                                           
*                                                                               
PROC61   DS    0H                                                               
         USING PIMHDREL,R2                                                      
         MVC   P,=132C' '          CLEAR PRINT LINE                             
*                                                                               
         OC    PIMHDRSQ,PIMHDRSQ                                                
         BZ    PROC63                                                           
         MVC   P(12),=C'SEQ NUMBER: '                                           
         EDIT  (B1,PIMHDRSQ),(3,P+12)                                           
         AP    INVCNT,=P'1'                                                     
*                                                                               
PROC62   OC    PIMINVDT,PIMINVDT                                                
         BZ    PROC63                                                           
         MVC   P+30(14),=C'INVOICE DATE: '                                      
         GOTO1 DATCON,DMCB,(3,PIMINVDT),(10,P+44)                               
*                                                                               
PROC63   OC    PIMSTDT,PIMSTDT                                                  
         BZ    PROC65                                                           
         MVC   P+55(12),=C'START DATE: '                                        
         GOTO1 DATCON,DMCB,(3,PIMSTDT),(10,P+67)                                
*                                                                               
PROC65   OC    PIMENDDT,PIMENDDT                                                
         BZ    PROC66                                                           
         MVC   P+78(10),=C'END DATE: '                                          
         GOTO1 DATCON,DMCB,(3,PIMENDDT),(10,P+88)                               
*                                                                               
PROC66   OC    PIMCREDT,PIMCREDT                                                
         BZ    PROC67                                                           
         MVC   P+99(20),=C'DATE INVOICE ADDED: '                                
         GOTO1 DATCON,DMCB,(3,PIMCREDT),(10,P+119)                              
*                                                                               
PROC67   DS    0H                                                               
         CLC   P,=132C' '          ANYTHING TO BE PRINTED?                      
         BE    PROC60                                                           
         CLI   QOPT3,C'Y'          ONE ONLY MED/CLIENT ?                        
         BNE   *+10                NO                                           
         MVC   P+18(11),PIMINVNO                                                
         BAS   RE,PRINTIT                                                       
         MVC   P,=132C' '          CLEAR PRINT LINE                             
         B     PROC60                                                           
         DROP  R2                                                               
*                                                                               
PROC70   DS    0H                                                               
         CLI   QOPT2,C'Y'          DUMP RECORD ?                                
         BNE   PROC72              NO                                           
*                                                                               
         MVC   P(07),=C'BEFORE:'                                                
         BAS   RE,DMPREC                                                        
*                                                                               
PROC72   DS    0H                                                               
         OI    KEY+25,X'80'        MARK FOR DELETION                            
         AP    DELCNT,=P'1'        COUNTING REC(S) MARKED FOR DEL               
         CLI   RCWRITE,C'N'                                                     
         BE    PROC75              TEST RUN - NO WRITE                          
*                                                                               
         GOTO1 WRT                                                              
         AP    WRTCNT,=P'1'                                                     
*                                                                               
PROC75   DS    0H                                                               
         CLI   QOPT2,C'Y'          DUMP RECORD ?                                
         BNE   PROC77                                                           
*                                                                               
         MVC   P(07),=C'AFTER :'                                                
         BAS   RE,DMPREC                                                        
         BAS   RE,PRINTIT          SKIP A LINE                                  
*                                                                               
PROC77   DS    0H                                                               
         B     PROC3               NEXT SEQ                                     
*                                                                               
*                                                                               
*                                                                               
PROC80   DS    0H                                                               
         CLI   KEY,X'FF'           END OF FILE                                  
         BE    EXIT                                                             
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+3,X'FF'         SKIP TO NEXT AGY/MED                         
         XC    KEY+4(28),KEY+4                                                  
         GOTO1 HIGH                                                             
         CLI   KEY,X'FF'           END OF FILE                                  
         BE    EXIT                                                             
         XC    KEY+4(28),KEY+4                                                  
         MVI   KEY+3,X'50'         INVOICE MATCHING RECORD                      
         B     PROC2                                                            
*                                                                               
*                                                                               
*                                                                               
RUNL     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         LA    R4,COUNTS                                                        
RUNL5    CLI   0(R4),X'FF'                                                      
         BE    RUNL10                                                           
         MVC   P(30),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+35(10),0(8,R4)                                                 
         BAS   RE,PRINTIT                                                       
         LA    R4,38(R4)           EACH COUNT ENTRY IS CL30+PL8                 
         B     RUNL5                                                            
*                                                                               
RUNL10   MVC   P,=132C' '          SKIP A LINE                                  
         BAS   RE,PRINTIT                                                       
         OC    AGYTAB,AGYTAB                                                    
         BZ    RUNLX                                                            
         MVC   P(20),=C'AGENCIES PROCESSED: '                                   
         BAS   RE,PRINTIT                                                       
*                                                                               
         MVC   P,=132C' '          CLEAR PRINT LINE                             
         LA    R4,AGYTAB                                                        
         LA    R5,P+22                                                          
         SR    R6,R6                                                            
*                                                                               
RUNL15   CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    RUNL20                                                           
         CLI   0(R4),0             NO MORE ENTRIES?                             
         BE    RUNL20                                                           
         CHI   R6,5                5 AGENCY PER LINE                            
         BL    RUNL18                                                           
         BAS   RE,PRINTIT                                                       
         MVC   P,=132C' '          CLEAR PRINT LINE                             
         LA    R5,P+22                                                          
         SR    R6,R6                                                            
*                                                                               
RUNL18   MVC   0(2,R5),0(R4)                                                    
         LA    R4,2(R4)                                                         
         LA    R5,4(R5)                                                         
         AHI   R6,1                AGENCY PER LINE COUNTER                      
         B     RUNL15                                                           
*                                                                               
RUNL20   CLC   P,=132C' '                                                       
         BE    *+8                                                              
         BAS   RE,PRINTIT                                                       
         CLI   AGYFLAG,C'Y'        TABLE IS FULL?                               
         BNE   RUNLX                                                            
         MVC   P,=132C' '          CLEAR PRINT LINE                             
         BAS   RE,PRINTIT                                                       
         MVC   P(95),=C'*** NOTE: THERE ARE MORE THAN 125 AGENCIES PROC+        
               CESSED, TABLE IS TOO SMALL TO HOLD ALL OF THEM.'                 
         BAS   RE,PRINTIT                                                       
*                                                                               
RUNLX    DS    0H                                                               
*                                                                               
*                                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
*                                                                               
*                                                                               
PRINTIT  NTR1                                                                   
         MVC   HEAD5+62(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+68(3),=C'YES'                                              
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
DMPREC   NTR1                                                                   
*NOP*    L     R5,AREC                                                          
         LA    R5,KEY                                                           
         GOTO1 HEXOUT,DMCB,(R5),P+010,031,=C'N'                                 
         MVC   P+80(31),0(R5)                                                   
         MVC   P+115(5),=C'D/A: '                                               
         GOTO1 HEXOUT,DMCB,KEY+27,P+120,4,=C'N'                                 
         BAS   RE,PRINTIT                                                       
         B     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
NXTEL20  IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NXTEL20                                                          
         LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
COUNTS   DS    0C                                                               
RECCNT   DS    PL8                                                              
         DC    CL30'REC(S) READ:'                                               
DELCNT   DS    PL8                                                              
         DC    CL30'REC(S) MARKED FOR DELETION:'                                
WRTCNT   DS    PL8                                                              
         DC    CL30'REC(S) ACTUALLY WRITTEN:'                                   
INVCNT   DS    PL8                                                              
         DC    CL30'NUMBER OF INVOICES:'                                        
         DC    X'FF'                                                            
*                                                                               
*                                                                               
*                                                                               
AGYFLAG  DS    X                                                                
AGYTAB   DS    CL250               TAB FOR 125 AGENCIES (125*2=250)             
AGYTABX  DS    X                   END OF TAB                                   
*                                                                               
SVPUB    DS    XL6                                                              
*                                                                               
*                                                                               
PP02WRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
ELEM     DS    XL30                                                             
FRSTSW   DS    XL1                                                              
TYPE     DS    XL1                                                              
PPGKEY   DS    CL64                                                             
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
BUFFBUFF DS    A                                                                
BUFFIO   DS    A                                                                
ITOT     DS    F                                                                
SKEY     DS    CL64                                                             
*                                                                               
*                                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PINVREC                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045PPREPMP02 01/12/09'                                      
         END                                                                    
