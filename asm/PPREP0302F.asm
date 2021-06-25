*          DATA SET PPREP0302F AT LEVEL 004 AS OF 05/01/02                      
*PHASE PP0302F,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
*INCLUDE BINSRCH2                                                               
         TITLE 'PP0302 - PRTFIX PROGRAM'                                        
*                                                                               
*   THIS PROGRAM WILL COPY AGENCY JW PUB RECORDS TO AGENCY FR PUB               
*   RECORDS FOR FORD CLIENTS. IT WILL ALSO COPY THE "APPROPRIATE"               
*   REP RECORDS FROM AGENCY JW TO AGENCY FR.                                    
*                                                                               
*      QOPT1     Y= PRINT PUBS WITH NO BUYS                                     
*      QOPT2     Y= DUMP FIRST 10 REPS                                          
*      QOPT3     Y= PRINT PUBLISHER AND REPS TO BE COPIED                       
*      QOPT5     Y= TEST RUN (DON'T MARK FILE)                                  
*      QOPT6     Y= DUMP FIRST 10 RECORDS (BEFORE AND AFTER)                    
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
**                                                                              
         CLI   MODE,PROCPUB                                                     
         BE    PROC                                                             
         CLI   MODE,FPUBREQ                                                     
         BE    REQF                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,LPUBREQ                                                     
         BE    RUNL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    EXIT                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         B     EXIT                                                             
*                                                                               
REQF     DS    0H                  FIRST PUB FOR REQUEST                        
         ZAP   INCNT,=P'0'         PUBS READ                                    
         ZAP   CPYCNT,=P'0'        PUBS COPIED                                  
         ZAP   LTLCNT,=P'0'        LTLRECS READ                                 
         ZAP   CLTLCNT,=P'0'       LTLRECS COPIED                               
         ZAP   LTLELEM,=P'0'       LTLREC ELEMENTS DELETED                      
         ZAP   ELEMCNT,=P'0'       ELEMENTS DELETED                             
         ZAP   REPCNT,=P'0'        REPS ADDED TO BINTBL                         
         ZAP   CREPCNT,=P'0'       REPS COPIED                                  
         ZAP   DUMPCNT,=P'0'                                                    
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         CLI   QOPT2,C'Y'          MEANS PRINT REP RECORDS                      
         BNE   *+8                                                              
         MVI   REPSW,C'Y'                                                       
*                            CLEAR BINSRCH TABLE OF REPS (MED/REP)              
         L     RE,BINATAB          A(TABLE)                                     
         L     RF,=A(5000)                                                      
         XCEF                                                                   
         XC    BINPARMS(4),BINPARMS                                             
         XC    BINCOUNT(4),BINCOUNT                                             
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         AP    INCNT,=P'1'                                                      
         XC    KEY,KEY             CHECK FOR BUYS FOR FORD CLIENTS              
         MVC   KEY(2),PUBKAGY                                                   
         MVC   KEY+2(1),PUBKMED                                                 
         MVI   KEY+3,X'21'         PASSIVE BUY                                  
         MVC   KEY+7(6),PUBKPUB                                                 
         LA    R3,CLIENTS          TABLE OF FORD CLIENTS                        
PROC2    MVC   KEY+4(3),0(R3)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY+4(3),CLIENTS+21       FORD CLIENT FOUND?                     
         BH    PROC3                     NO - NO PUB COPY                       
         CLC   KEY(13),KEYSAVE     SAME CLIENT AND PUB ?                        
         BE    PROC2D              YES - GO TEST DATE                           
PROC2B   LA    R3,3(R3)            NEXT CLIENT                                  
         CLI   0(R3),X'FF'         END OF CLIENTS?                              
         BE    PROC3               YES - NO PUB COPY                            
         MVC   KEY,KEYSAVE                                                      
         B     PROC2               TRY NEXT CLIENT                              
PROC2D   CLC   KEY+16(3),=X'5F0000'      BUY AFTER 12/31/94 ?                   
         BH    PROC4                     YES - GO COPY PUB                      
         GOTO1 SEQ                                                              
         CLC   KEY(13),KEYSAVE     SAME CLIENT AND PUB ?                        
         BE    PROC2D              YES - GO TEST DATE                           
         B     PROC2B              TRY NEXT CLIENT                              
*                                                                               
PROC3    DS    0H                                                               
         CLI   QOPT1,C'Y'          PRINT NO BUY PUBS ?                          
         BNE   EXIT                NO - NEXT PUB                                
         MVC   P(5),=C'PUB# '                                                   
         GOTO1 PUBEDIT,DMCB,PUBREC+1,(0,P+5)                                    
         MVC   P+50(33),=C'** NO FORD BUYS AFTER 12/31/94 **'                   
         BAS   RE,RPRT                                                          
         B     EXIT                NEXT PUB                                     
*                                                                               
*                                                                               
PROC4    DS    0H                  COPY THE PUB RECORD TO AGY FR                
         MVC   P(5),=C'PUB# '                                                   
         GOTO1 PUBEDIT,DMCB,PUBREC+1,(0,P+5)                                    
         MVC   P+80(37),=C'** FORD BUYS FOUND - PUB CONVERTED **'               
         MVC   P+25(20),PUBNAME                                                 
         OC    PUBZNAME,PUBZNAME                                                
         BZ    PROC4A                                                           
         MVC   P+50(5),=C'ZONE:'                                                
         MVC   P+55(20),PUBZNAME                                                
PROC4A   BAS   RE,RPRT                                                          
         MVI   DUMPSW,0                                                         
         MVI   DELSW,0                                                          
         AP    CPYCNT,=P'1'        PUB COPY COUNT                               
PROC4A1  LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'08'        CHECK PAY ADDRESSES                          
PROC4A2  BAS   RE,NEXTEL                                                        
         BNE   PROC4B              NO (MORE) PAY ADDRESSES                      
         CLC   2(3,R2),=X'FFFFFF'  AGENCY ADDRESS ?                             
         BE    PROC4A2             YES - KEEP                                   
         CLC   2(2,R2),=X'FFD1'    OFFICE J ?                                   
         BE    PROC4A2             YES - KEEP                                   
         CLC   2(2,R2),=X'FFF5'    OFFICE 5 ?                                   
         BNE   PROC4A3             NO - CHECK CLIENTS                           
         MVC   2(2,R2),=X'FFD1'    YES - CHANGE TO OFFICE J                     
         B     PROC4A2             CHECK NEXT ELEMENT                           
PROC4A3  LA    R3,CLIENTS          TABLE OF FORD CLIENTS                        
PROC4A4  CLC   2(3,R2),0(R3)       FORD CLIENT?                                 
         BE    PROC4A2             YES - KEEP                                   
         LA    R3,3(R3)            NEXT CLIENT                                  
         CLI   0(R3),X'FF'         END OF FORD CLIENTS?                         
         BNE   PROC4A4             NO - CHECK NEXT CLIENT                       
         BAS   RE,DELELEM          YES - DELETE ELEMENT (NOT FORD)              
         B     PROC4A1             CHECK NEXT ELEMENT                           
*                                                                               
PROC4B   LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'09'        CHECK TRAFFIC ADDRESSES                      
PROC4B2  BAS   RE,NEXTEL                                                        
         BNE   PROC4C              NO (MORE) TRAFFIC ADDRESSES                  
         CLC   2(3,R2),=X'FFFFFF'  AGENCY ADDRESS ?                             
         BE    PROC4B2             YES - KEEP                                   
         CLC   2(2,R2),=X'FFD1'    OFFICE J ?                                   
         BE    PROC4B2             YES - KEEP                                   
         CLC   2(2,R2),=X'FFF5'    OFFICE 5 ?                                   
         BNE   PROC4B3             NO - CHECK CLIENTS                           
         MVC   2(2,R2),=X'FFD1'    YES - CHANGE TO OFFICE J                     
         B     PROC4B2             CHECK NEXT ELEMENT                           
PROC4B3  LA    R3,CLIENTS          TABLE OF FORD CLIENTS                        
PROC4B4  CLC   2(3,R2),0(R3)       FORD CLIENT?                                 
         BE    PROC4B2             YES - KEEP                                   
         LA    R3,3(R3)            NEXT CLIENT                                  
         CLI   0(R3),X'FF'         END OF FORD CLIENTS?                         
         BNE   PROC4B4             NO - CHECK NEXT CLIENT                       
         BAS   RE,DELELEM          YES - DELETE ELEMENT (NOT FORD)              
         B     PROC4B              CHECK NEXT ELEMENT                           
*                                                                               
PROC4C   LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'0A'        CHECK CONTRACT ADDRESSES                     
PROC4C2  BAS   RE,NEXTEL                                                        
         BNE   PROC4D              NO (MORE) CONTRACT ADDRESSES                 
         CLC   2(3,R2),=X'FFFFFF'  AGENCY ADDRESS ?                             
         BE    PROC4C2             YES - KEEP                                   
         CLC   2(2,R2),=X'FFD1'    OFFICE J ?                                   
         BE    PROC4C2             YES - KEEP                                   
         CLC   2(2,R2),=X'FFF5'    OFFICE 5 ?                                   
         BNE   PROC4C3             NO - CHECK CLIENTS                           
         MVC   2(2,R2),=X'FFD1'    YES - CHANGE TO OFFICE J                     
         B     PROC4C2             CHECK NEXT ELEMENT                           
PROC4C3  LA    R3,CLIENTS          TABLE OF FORD CLIENTS                        
PROC4C4  CLC   2(3,R2),0(R3)       FORD CLIENT?                                 
         BE    PROC4C2             YES - KEEP                                   
         LA    R3,3(R3)            NEXT CLIENT                                  
         CLI   0(R3),X'FF'         END OF FORD CLIENTS?                         
         BNE   PROC4C4             NO - CHECK NEXT CLIENT                       
         BAS   RE,DELELEM          YES - DELETE ELEMENT (NOT FORD)              
         B     PROC4C              CHECK NEXT ELEMENT                           
*                                                                               
PROC4D   LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'0B'        CHECK SHIPPING ADDRESSES                     
PROC4D2  BAS   RE,NEXTEL                                                        
         BNE   PROC4E              NO (MORE) SHIPPING ADDRESSES                 
         CLC   2(3,R2),=X'FFFFFF'  AGENCY ADDRESS ?                             
         BE    PROC4D2             YES - KEEP                                   
         CLC   2(2,R2),=X'FFD1'    OFFICE J ?                                   
         BE    PROC4D2             YES - KEEP                                   
         CLC   2(2,R2),=X'FFF5'    OFFICE 5 ?                                   
         BNE   PROC4D3             NO - CHECK CLIENTS                           
         MVC   2(2,R2),=X'FFD1'    YES - CHANGE TO OFFICE J                     
         B     PROC4D2             CHECK NEXT ELEMENT                           
PROC4D3  LA    R3,CLIENTS          TABLE OF FORD CLIENTS                        
PROC4D4  CLC   2(3,R2),0(R3)       FORD CLIENT?                                 
         BE    PROC4D2             YES - KEEP                                   
         LA    R3,3(R3)            NEXT CLIENT                                  
         CLI   0(R3),X'FF'         END OF FORD CLIENTS?                         
         BNE   PROC4D4             NO - CHECK NEXT CLIENT                       
         BAS   RE,DELELEM          YES - DELETE ELEMENT (NOT FORD)              
         B     PROC4D              CHECK NEXT ELEMENT                           
*                                                                               
PROC4E   LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'11'        CHECK SUPPLEMENTAL ADDRESS                   
PROC4E2  BAS   RE,NEXTEL                                                        
         BNE   PROC4F              NO (MORE) SUPPLEMENTAL ADDRESS               
         CLC   2(3,R2),=X'FFFFFF'  AGENCY ADDRESS ?                             
         BE    PROC4E2             YES - KEEP                                   
         CLC   2(2,R2),=X'FFD1'    OFFICE J ?                                   
         BE    PROC4E2             YES - KEEP                                   
         CLC   2(2,R2),=X'FFF5'    OFFICE 5 ?                                   
         BNE   PROC4E3             NO - CHECK CLIENTS                           
         MVC   2(2,R2),=X'FFD1'    YES - CHANGE TO OFFICE J                     
         B     PROC4E2             CHECK NEXT ELEMENT                           
PROC4E3  LA    R3,CLIENTS          TABLE OF FORD CLIENTS                        
PROC4E4  CLC   2(3,R2),0(R3)       FORD CLIENT?                                 
         BE    PROC4E2             YES - KEEP                                   
         LA    R3,3(R3)            NEXT CLIENT                                  
         CLI   0(R3),X'FF'         END OF FORD CLIENTS?                         
         BNE   PROC4E4             NO - CHECK NEXT CLIENT                       
         BAS   RE,DELELEM          YES - DELETE ELEMENT (NOT FORD)              
         B     PROC4E              CHECK NEXT ELEMENT                           
*                                                                               
PROC4F   LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'14'        CHECK CLIENT/OFFICE REPS                     
PROC4F2  BAS   RE,NEXTEL                                                        
         BNE   PROC5               NO (MORE) CLIENT/OFFICE REPS                 
         CLC   2(3,R2),=X'FFFFFF'  AGENCY ?                                     
         BE    PROC4F2             YES - KEEP                                   
         CLC   2(2,R2),=X'FFD1'    OFFICE J ?                                   
         BE    PROC4F2             YES - KEEP                                   
         CLC   2(2,R2),=X'FFF5'    OFFICE 5?                                    
         BNE   PROC4F3             NO - CHECK CLIENTS                           
         MVC   2(2,R2),=X'FFD1'    YES - CHANGE TO OFFICE J                     
         B     PROC4F2             CHECK NEXT ELEMENT                           
PROC4F3  LA    R3,CLIENTS          TABLE OF FORD CLIENTS                        
PROC4F4  CLC   2(3,R2),0(R3)       FORD CLIENT?                                 
         BE    PROC4F2             YES - KEEP                                   
         LA    R3,3(R3)            NEXT CLIENT                                  
         CLI   0(R3),X'FF'         END OF FORD CLIENTS?                         
         BNE   PROC4F4             NO - CHECK NEXT CLIENT                       
         BAS   RE,DELELEM          YES - DELETE ELEMENT (NOT FORD)              
         B     PROC4F              CHECK NEXT ELEMENT                           
*                                                                               
*                                                                               
PROC5    DS    0H                  ADD PUB FOR FR                               
         LA    R0,PUBREC                                                        
         ST    R0,AREC                                                          
*****    CLI   QOPT6,C'Y'          DUMP RECORD ?                                
*****    BNE   PROC10              NO - GO ADD                                  
*****    CLI   DUMPSW,1            "BEFORE" ALREADY DUMPED ?                    
*****    BE    PROC10              YES - SKIP                                   
*****    AP    DUMPCNT,=P'1'                                                    
*****    CP    DUMPCNT,=P'10'      10 RECORDS DUMPED ?                          
*****    BNH   DUMPBEF             NO                                           
         B     PROC10              YES - SKIP DUMP                              
*                                                                               
DUMPBEF  BAS   RE,RPRT                                                          
         MVC   P+55(12),=C'** BEFORE **'                                        
*****    BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
PROC10   DS    0H                                                               
         BAS   RE,CLEARREC         CLEAR END OF RECORD                          
         XC    KEY,KEY                                                          
         MVC   PUBKAGY,=C'FR'      NEW FORD AGENCY                              
         MVC   KEY(25),PUBREC                                                   
         CLI   RCWRITE,C'N'        WRITE RECORD ?                               
         BE    PROC12              NO                                           
*                                                                               
         GOTO1 ADDPUB                                                           
*                                                                               
PROC12   DS    0H                                                               
         CLI   QOPT6,C'Y'          DUMP RECORDS ?                               
         BNE   PROC20              NO                                           
         CP    DUMPCNT,=P'10'      10 RECORDS DUMPED ?                          
         BH    PROC20              YES                                          
         CLI   DELSW,1             ELEMENT(S) DELETED ?                         
         BNE   PROC20              NO                                           
*                                                                               
PROC14   MVC   P+55(12),=C'** AFTER ***'                                        
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
PROC20   DS    0H                  CHECK FOR LTLREC                             
         MVI   LTLSW,0                                                          
         L     R2,ALTLREC                                                       
         ST    R2,AREC                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(9),PUBREC                                                    
         MVC   KEY+7(2),=C'JW'     RESTORE "READ" AGENCY                        
         MVI   KEY+9,X'85'                                                      
         GOTO1 HIGHPUB                                                          
         CLC   KEY(10),KEYSAVE     LTLREC FOUND?                                
         BNE   PROC40              NO - GO PROCESS REP TABLE                    
         AP    LTLCNT,=P'1'                                                     
*****    MVC   P+10(22),=C'** LITTLE REC FOUND **'                              
*****    BAS   RE,RPRT                                                          
         GOTO1 GETLTL                                                           
*                                                                               
PROC24   L     R2,ALTLREC                                                       
         LA    R2,33(R2)                                                        
         MVI   ELCODE,X'71'        CHECK CLT-DIV-REG-DST                        
         CLI   0(R2),X'71'                                                      
         BE    PROC24D                                                          
PROC24B  BAS   RE,NEXTEL                                                        
         BNE   PROC30              NO (MORE) X'71' ELEMENTS                     
PROC24D  LA    R3,CLIENTS          TABLE OF FORD CLIENTS                        
PROC24F  CLC   2(3,R2),0(R3)       FORD CLIENT?                                 
         BNE   PROC24K             NO - CHECK NEXT CLIENT                       
         MVI   LTLSW,1             COPY THIS LTLREC                             
         B     PROC24B             NEXT ELEMENT                                 
PROC24K  LA    R3,3(R3)            NEXT CLIENT                                  
         CLI   0(R3),X'FF'         END OF FORD CLIENTS?                         
         BNE   PROC24F             NO - CHECK NEXT CLIENT                       
         L     R5,ALTLREC                                                       
         GOTO1 RECUP,DMCB,(1,(R5)),(R2),0       DELETE ELEMENT                  
         AP    LTLELEM,=P'1'                                                    
         B     PROC24              CHECK NEXT ELEMENT                           
*                                                                               
PROC30   DS    0H                                                               
         CLI   LTLSW,1             COPY THIS LTLREC?                            
         BNE   PROC40              NO - GO DO REP TABLE                         
         L     R2,ALTLREC                                                       
         ST    R2,AREC                                                          
*                                  CLEAR END OF RECORD                          
         MVC   HALF,25(R2)                                                      
         LH    R1,HALF                                                          
         LR    RE,R2                                                            
         LR    RF,RE                                                            
         AR    RE,R1                                                            
         LA    RF,250(RF)                                                       
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   7(2,R2),=C'FR'       NEW FORD AGENCY                             
         MVC   KEY(25),0(R2)                                                    
         AP    CLTLCNT,=P'1'                                                    
         MVC   P+10(26),=C'** LITTLE REC CONVERTED **'                          
         BAS   RE,RPRT                                                          
         CLI   RCWRITE,C'N'        WRITE RECORD ?                               
         BE    PROC40              NO                                           
*                                                                               
         GOTO1 ADDPUB                                                           
*                                                                               
PROC40   DS    0H                  BUILD TABLE OF REPS                          
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(1),PUBKMED                                                  
*                                                                               
         LA    R2,PUBREC+33                                                     
         CLI   0(R2),X'10'                                                      
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         LA    R4,P+3              FOR POSSIBLE PUBL AND REP PRINTING           
         USING PUBNAMEL,R2                                                      
         OC    PUBPLSH,PUBPLSH     ANY PUBLISHER?                               
         BZ    PROC50              NO                                           
         MVC   WORK+1(4),PUBPLSH   YES - ADD TO TABLE                           
         GOTO1 =V(BINSRCH),BINPARMS,(X'01',WORK)                                
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         AP    REPCNT,=P'1'                                                     
         MVC   0(5,R4),=C'PUBL='   R4 POINTING TO P                             
         MVC   5(4,R4),PUBPLSH                                                  
         LA    R4,11(R4)                                                        
         DROP  R2                                                               
*                                                                               
PROC50   MVI   ELCODE,X'14'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   PROC50X         NO (MORE) REP ELEMENTS - DONE WITH PUB           
         USING PPDUMD14,R2                                                      
         OC    PUBPAREP,PUBPAREP    ANY PAYING REP?                             
         BZ    PROC50B              NO                                          
         MVC   WORK+1(4),PUBPAREP   YES - ADD TO TABLE                          
         GOTO1 =V(BINSRCH),BINPARMS,(X'01',WORK)                                
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   0(6,R4),=C'PAREP='    R4 POINTING TO P                           
         MVC   6(4,R4),PUBPAREP                                                 
         LA    R4,12(R4)                                                        
         AP    REPCNT,=P'1'                                                     
*                                                                               
PROC50B  OC    PUBTRREP,PUBTRREP    ANY TRAFFIC REP?                            
         BZ    PROC50D              NO                                          
         MVC   WORK+1(4),PUBTRREP   YES - ADD TO TABLE                          
         GOTO1 =V(BINSRCH),BINPARMS,(X'01',WORK)                                
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   0(6,R4),=C'TRREP='    R4 POINTING TO P                           
         MVC   6(4,R4),PUBTRREP                                                 
         LA    R4,12(R4)                                                        
         AP    REPCNT,=P'1'                                                     
*                                                                               
PROC50D  OC    PUBCNREP,PUBCNREP    ANY CONTRACT REP?                           
         BZ    PROC50               NO - CHECK NEXT ELEMENT                     
         MVC   WORK+1(4),PUBCNREP   YES - ADD TO TABLE                          
         GOTO1 =V(BINSRCH),BINPARMS,(X'01',WORK)                                
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   0(6,R4),=C'CNREP='    R4 POINTING TO P                           
         MVC   6(4,R4),PUBCNREP                                                 
         LA    R4,12(R4)                                                        
         AP    REPCNT,=P'1'                                                     
         B     PROC50               CHECK NEXT ELEMENT                          
*                                                                               
PROC50X  DS    0H                                                               
         CLC   P(10),SPACES        ANYTHING TO PRINT ?                          
         BNH   EXIT                NO - DONE WITH PUB                           
         CLI   QOPT3,C'Y'          PRINT PUBL AND REPS ?                        
         BE    PROC50XX            YES                                          
         XC    P(132),P            CLEAR LINE                                   
         B     EXIT                                                             
PROC50XX BAS   RE,RPRT             PRINT PUBL-REP LINE                          
         B     EXIT                                                             
*                                                                               
*                                                                               
RUNL     DS    0H                                                               
         MVC   WORK(5),=5X'FF'      END OF TABLE                                
         GOTO1 =V(BINSRCH),BINPARMS,(X'01',WORK)                                
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
*                                                                               
*                          COPY REP RECORDS BASED ON TABLE ENTRIES              
         XC    WORK(5),WORK                                                     
         GOTO1 =V(BINSRCH),BINPARMS,(X'02',WORK)                                
         CLI   0(R1),1                                                          
         BNE   *+6                                                              
         DC    H'0'                MUST BE FOUND                                
         ZICM  R3,1(R1),3          POINT R3 TO TABLE - A(FOUND RECORD)          
*                                                                               
RUNL10   CLI   0(R3),X'FF'         END OF TABLE?                                
         BE    RUNL40              YES - FINISH UP                              
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=C'JW'       JWT AGENCY                                   
         MVC   KEY+2(1),0(R3)      MEDIA                                        
         MVI   KEY+3,X'11'         REP RECORD CODE                              
         MVC   KEY+4(4),1(R3)      REP CODE                                     
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE FOUND                                
*                                  COPY REP RECORD TO AGENCY FR                 
         LA    R0,PREPREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETREP                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   PREPKAGY,=C'FR'     NEW AGENCY                                   
         MVC   KEY(25),PREPREC                                                  
         AP    CREPCNT,=P'1'                                                    
*                                                                               
         CLI   REPSW,C'Y'          DUMP RECORD ?                                
         BNE   RUNL28              NO                                           
         CP    CREPCNT,=P'10'      10 REPS DUMPED ?                             
         BH    RUNL28              YES - NO MORE                                
*                                                                               
         BAS   RE,RPRT                                                          
         MVC   P+55(14),=C'** REP COPY **'                                      
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
RUNL28   CLI   RCWRITE,C'N'        WRITE RECORD ?                               
         BE    RUNL30              NO                                           
*                                                                               
         GOTO1 ADDPRT                                                           
*                                                                               
RUNL30   LA    R3,5(R3)            BUMP TO NEXT TABLE ENTRY                     
         B     RUNL10                                                           
*                                                                               
RUNL40   MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         LA    R4,COUNTS                                                        
RUNL50   CLI   0(R4),X'FF'                                                      
         BE    RUNL100                                                          
         MVC   P(15),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,23(R4)                                                        
         B     RUNL50                                                           
*                                                                               
RUNL100  DS    0H                                                               
         MVC   P(11),=C'TABLE COUNT'                                            
         L     R5,BINCOUNT                                                      
         EDIT  (R5),(10,P+20),FILL=0                                            
         GOTO1 REPORT                                                           
*                                                                               
*                                                                               
RUNLX    DS    0H                                                               
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
DELELEM  NTR1                ***** DELETE PUB ADDRESS ELEMENT                   
         CLI   QOPT6,C'Y'          DUMP RECORDS ?                               
         BNE   DELELD              NO                                           
*                                                                               
         CLI   DUMPSW,1            "BEFORE" DUMPED ?                            
         BE    DELELD              YES                                          
         MVI   DUMPSW,1            NO - DO NOW                                  
*                                                                               
         AP    DUMPCNT,=P'1'                                                    
         CP    DUMPCNT,=P'10'      10 RECORDS DUMPED ?                          
         BH    DELELD              YES                                          
*****    BAS   RE,RPRT                                                          
         MVC   P+50(22),=C'** BEFORE DELETIONS **'                              
         BAS   RE,RPRT                                                          
         LA    R0,PUBREC                                                        
         ST    R0,AREC                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
DELELD   GOTO1 RECUP,DMCB,(1,PUBREC),(R2),0       DELETE ELEMENT                
         AP    ELEMCNT,=P'1'                                                    
         MVI   DELSW,1             AN ELEMENT HAS BEEN DELETED                  
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
CLEARREC NTR1                    CLEAR END OF 4K MAX LENGTH RECORD              
         MVC   HALF,PUBREC+25                                                   
         LH    R1,HALF                                                          
         LA    RE,PUBREC                                                        
         LR    RF,RE                                                            
         AR    RE,R1                                                            
         LA    RF,4000(RF)                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         B     EXIT                                                             
*                             LINK TO REPORT                                    
         SPACE 2                                                                
         DS    0F                                                               
RPRT     NTR1                                                                   
         SPACE 2                                                                
         MVI   RCSUBPRG,20                                                      
         MVC   HEAD4+62(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD4+68(3),=C'YES'                                              
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
*                                                                               
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NEXTEL+2                                                         
         LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
DMPKEY   NTR1                                                                   
         SPACE 2                                                                
         LA    R5,KEY                                                           
         LA    R2,25                                                            
         GOTO1 HEXOUT,DMCB,(R5),P+01,(R2),=C'N'                                 
*                                                                               
         MVC   WORK(25),0(R5)                                                   
         TR    WORK(25),TRTAB                                                   
         MVC   P+75(25),WORK                                                    
         B     EXIT                                                             
         SPACE 2                                                                
DMPREC   NTR1                                                                   
         SPACE 1                                                                
         L     R5,AREC                                                          
         MVC   HALF,25(R5)        RECORD LENGTH                                 
         LH    R2,HALF                                                          
         LA    R3,0(R5,R2)                                                      
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   EXIT                                                             
         CH    R4,=H'32'                                                        
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 HEXOUT,DMCB,(R5),WORK,(R4),=C'N'                                 
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+37(8),WORK+32                                                  
         MVC   P+46(8),WORK+40                                                  
         MVC   P+55(8),WORK+48                                                  
         MVC   P+64(8),WORK+56                                                  
*                                                                               
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+75(0),WORK                                                     
         LA    R4,1(R4)                                                         
         BAS   RE,RPRT                                                          
         LA    R5,0(R5,R4)                                                      
         B     DMPREC2                                                          
         SPACE 3                                                                
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     40-4F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
COUNTS   DS    0C                                                               
INCNT    DS    PL8                                                              
         DC    CL15'PUB RECS READ'                                              
CPYCNT   DS    PL8                                                              
         DC    CL15'PUB RECS COPIED'                                            
ELEMCNT  DS    PL8                                                              
         DC    CL15'ELEMENTS DELETD'                                            
LTLCNT   DS    PL8                                                              
         DC    CL15'LTL RECS READ'                                              
CLTLCNT  DS    PL8                                                              
         DC    CL15'LTL RECS COPIED'                                            
LTLELEM  DS    PL8                                                              
         DC    CL15'LTL ELEMS DELTD'                                            
REPCNT   DS    PL8                                                              
         DC    CL15'REPS TESTED'                                                
CREPCNT  DS    PL8                                                              
         DC    CL15'REPS COPIED'                                                
         DC    X'FF'                                                            
DUMPCNT  DS    PL8                                                              
*                                                                               
SAVKEY   DS    CL32                                                             
CNT24    DS    PL4                                                              
CNT70    DS    PL4                                                              
OLDDATE  DS    CL3                                                              
ERRSW    DS    CL1                                                              
PUBSW    DS    CL1                                                              
CHKGST   DS    CL1                                                              
DDSBLD   DS    CL1                                                              
DATETYP  DS    CL1                                                              
KEYPSW   DS    CL1                                                              
*                                                                               
PUBNET   DS    PL8                                                              
PUBCASH  DS    PL8                                                              
PUBMYTAX DS    PL8                                                              
PUBBASIS DS    PL8                                                              
PUBGSTT  DS    PL8                                                              
PUBGSTTP DS    PL8                                                              
*                                                                               
OUTSPC   DS    CL40                                                             
*                                                                               
CLIENTS  DC    C'FCCFCDFCPFD FDMFDTFFONAO',X'FF'                                
*                                                                               
BINPARMS DC    A(0)                                                             
BINATAB  DC    A(TABLE)                                                         
BINCOUNT DC    A(0)                RECORD COUNT                                 
         DC    A(5)                LENGTH                                       
         DC    AL1(0),AL3(5)                                                    
BINMAX   DC    A(1000)             MAX NUMBER OF RECORDS                        
*                                                                               
TABLE    DS    CL5000                                                           
         SPACE 2                                                                
PP02WRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
ELEM     DS    XL30                                                             
FRSTSW   DS    XL1                                                              
DUMPSW   DS    XL1                                                              
LTLSW    DS    XL1                                                              
DELSW    DS    XL1                                                              
REPSW    DS    XL1                                                              
TYPE     DS    XL1                                                              
PPGKEY   DS    CL64                                                             
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
BUFFBUFF DS    A                                                                
BUFFIO   DS    A                                                                
ITOT     DS    F                                                                
SKEY     DS    CL64                                                             
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004PPREP0302F05/01/02'                                      
         END                                                                    
