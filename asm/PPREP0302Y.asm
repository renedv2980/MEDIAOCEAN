*          DATA SET PPREP0302Y AT LEVEL 035 AS OF 10/19/99                      
*PHASE PP0302Y,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PP0302 - PRTFIX PROGRAM'                                        
*                                                                               
*   THIS PROGRAM WILL READ BILL RECORDS AND SEARCH FOR THE PBILESTE             
*   FIELD.  PRINT OUT HEX AND CHAR FORMAT OF PBILESTE IF IT IS NOT              
*   NULLS (ALONG WITH THE KEY AS WELL).                                         
*                                                                               
*      QOPT4     Y MEANS NEW PAGE FOR SUMMARY (LAST RUN)                        
*                                                                               
PP0302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP0302                                                         
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
*                                                                               
*                                                                               
RUNF     DS    0H                                                               
         ZAP   RECCNT,=P'0'        RECS READ                                    
         ZAP   PRTCNT,=P'0'        RECS PRINTED                                 
         ZAP   SKPCNT,=P'0'        RECS SKIPPED                                 
         MVI   PRTFLAG,C' '        RECORD GET PRINTED FLAG                      
         XC    AGYTAB,AGYTAB       AGENCIES TO BE PROCESSED                     
         MVI   AGYTABX,X'FF'       END OF AGENCY TABLE                          
         MVI   AGYFLAG,0           AGYTAB FLAG FOR TAB IS FULL                  
         B     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
PROC     DS    0H                  FIRST BUY FOR REQUEST                        
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         LA    R0,PBILLREC                                                      
         ST    R0,AREC                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY       START AT AGENCY/MEDIA                       
         MVI   KEY+3,X'08'          BILL RECORD CODE                            
*                                                                               
PROC2    GOTO1 HIGH                                                             
         B     PROC5                                                            
*                                                                               
PROC3    GOTO1 SEQ                                                              
*                                                                               
PROC5    CLC   KEY(4),KEYSAVE      JUST CHECK AGY/MEDIA RECORD CODE             
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
PROC50   DS    0H                                                               
*                                                                               
         GOTO1 GETPRT                                                           
*                                                                               
         LA    R2,PBILLREC+33      SEE IF FIRST ELEM IS BILL ELEM               
         CLI   0(R2),X'08'                                                      
         BE    PROC61                                                           
PROC60   MVI   ELCODE,X'08'        BILL ELEM                                    
         BAS   RE,NEXTEL                                                        
         BNE   PROC70                                                           
*                                                                               
PROC61   DS    0H                                                               
         USING PBILLEL,R2                                                       
         MVC   P,=132C' '          CLEAR PRINT LINE                             
*                                                                               
         OC    PBILESTE,PBILESTE                                                
         BZ    PROC67                                                           
*                                                                               
         AP    PRTCNT,=P'1'        RECORDS PRINTED COUNTER                      
         MVI   PRTFLAG,C'Y'        RECORD GET PRINTED                           
         BAS   RE,DMPREC                                                        
*                                                                               
         MVC   P,=132C' '          CLEAR PRINT LINE                             
         MVC   P+00(08),=C'EBCDIC: '                                            
         MVC   P+10(02),PBILESTE                                                
         MVC   P+25(08),=C'HEXOUT: '                                            
         GOTO1 HEXOUT,DMCB,PBILESTE,P+33,2,=C'N'                                
         BAS   RE,PRINTIT                                                       
         MVC   P,=132C' '          CLEAR PRINT LINE                             
         BAS   RE,PRINTIT                                                       
         DROP  R2                                                               
*                                                                               
PROC67   DS    0H                                                               
         B     PROC60                                                           
*                                                                               
PROC70   DS    0H                                                               
         CLI   PRTFLAG,C'Y'        RECORD GET PRINTED?                          
         BE    *+14                                                             
         AP    SKPCNT,=P'1'        RECORD IS SKIPPED                            
         MVI   PRTFLAG,C' '        RESET FLAG                                   
         B     PROC3               NEXT SEQ                                     
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
         MVI   KEY+3,X'08'         BILL RECORD                                  
         B     PROC2                                                            
*                                                                               
*                                                                               
*                                                                               
RUNL     DS    0H                                                               
         CLI   QOPT4,C'Y'          START AS A NEW PAGE?                         
         BNE   *+8                                                              
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
         MVC   P(21),=C'AGENCIES PROCCESSED: '                                  
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
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
DMPREC   NTR1                                                                   
         L     R5,AREC                                                          
         GOTO1 HEXOUT,DMCB,(R5),P+010,033,=C'N'                                 
         MVC   P+80(33),0(R5)                                                   
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
PRTCNT   DS    PL8                                                              
         DC    CL30'REC(S) PRINTED:'                                            
SKPCNT   DS    PL8                                                              
         DC    CL30'REC(S) SKIPPED:'                                            
         DC    X'FF'                                                            
*                                                                               
*                                                                               
*                                                                               
AGYFLAG  DS    X                                                                
AGYTAB   DS    CL250               TAB FOR 125 AGENCIES (125*2=250)             
AGYTABX  DS    X                   END OF TAB                                   
*                                                                               
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
PRTFLAG  DS    C                                                                
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
**PAN#1  DC    CL21'035PPREP0302Y10/19/99'                                      
         END                                                                    
