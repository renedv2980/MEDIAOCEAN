*          DATA SET ACLFM14    AT LEVEL 023 AS OF 05/01/02                      
*PHASE T60314A,+0                                                               
         TITLE 'BATCH ENQUIRY FACILITY WITHIN LFM'                              
T60314   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**LFM14*                                                       
         PRINT NOGEN                                                            
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(5,0),(1,TODAY)                                      
         EJECT                                                                  
*              CHECK AND EXPAND INPUT FIELDS                                    
         SPACE 3                                                                
         CLI   LOGACT,C'I'                                                      
         BE    BE1                                                              
*&&US*&& FOUT  LOGACTH,=C'INQUIRY',7                                            
*&&UK*&& FOUT  LOGACTH,=C'ENQUIRY',7                                            
         SPACE 2                                                                
BE1      LA    R2,LOGINPH          INPUT TYPE                                   
         MVI   ERROR,X'FE'                                                      
         CLI   MODE,BUILDKEY                                                    
         BNE   XIT                                                              
         MVI   FILTTYPE,0                                                       
         CLI   5(R2),0                                                          
         BE    BE2                                                              
         GOTO1 ANY                                                              
         CLC   8(3,R2),=C'ALL'                                                  
         BE    BE2                                                              
         GOTO1 NUMERIC                                                          
         STC   R1,FILTTYPE                                                      
         SPACE 2                                                                
BE2      LA    R2,LOGSTUSH                                                      
         MVI   FILTSTAT,0                                                       
         CLI   5(R2),0                                                          
         BE    BE4                                                              
         GOTO1 ANY                                                              
         CLC   8(3,R2),=C'ALL'                                                  
         BE    BE4                                                              
         MVI   FILTSTAT,C'Y'                                                    
         CLC   8(2,R2),=C'OK'                                                   
         BE    BE4                                                              
         MVI   FILTSTAT,C'N'                                                    
         CLC   8(3,R2),=C'NOT'                                                  
         BE    BE4                                                              
         MVI   FILTSTAT,C'U'                                                    
         CLC   8(2,R2),=C'UP'                                                   
         BE    BE4                                                              
         MVI   FILTSTAT,0                                                       
         SPACE 2                                                                
BE4      LA    R2,LOGFILTH                                                      
         MVC   FILTFILT,8(R2)                                                   
         TM    LOGINPH+4,X'20'     ALWAYS START FROM FIRST BATCH                
         BO    *+10                IF EITHER FILTER HAS BEEN INPUT              
         XC    PREVKEY,PREVKEY                                                  
         OI    LOGINPH+4,X'20'                                                  
         TM    LOGSTUSH+4,X'20'                                                 
         BO    *+10                                                             
         XC    PREVKEY,PREVKEY                                                  
         OI    LOGSTUSH+4,X'20'                                                 
         TM    LOGFILTH+4,X'20'                                                 
         BO    *+10                                                             
         XC    PREVKEY,PREVKEY                                                  
         OI    LOGFILTH+4,X'20'                                                 
         EJECT                                                                  
*              READ BATCHES AND OUTPUT                                          
         SPACE 3                                                                
         USING ACBKEYD,R4                                                       
         LA    R2,LOGLINEH                                                      
         LA    R4,KEY                                                           
         ZAP   TOTBAT,=P'0'                                                     
         ZAP   TOTBIT,=P'0'                                                     
         ZAP   TOTBCH,=P'0'                                                     
         ZAP   TOTDIT,=P'0'                                                     
         ZAP   TOTDCH,=P'0'                                                     
         ZAP   TOTITM,=P'0'                                                     
         ZAP   BATCASH,=P'0'                                                    
         ZAP   BATITEM,=P'0'                                                    
         ZAP   BATDITEM,=P'0'                                                   
         MVI   BATTYP,0                                                         
         OC    PREVKEY,PREVKEY     DID WE SAVE A KEY                            
         BZ    ME12                                                             
         MVC   KEY,PREVKEY         YES - GET NEXT                               
         MVC   KEYSAVE,KEY                                                      
         XC    IO(42),IO                                                        
         MVC   IO(32),KEY                                                       
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCOUNT',IO,IO,              X        
               (TERMINAL,DMWORK)                                                
ME10     GOTO1 SEQ                                                              
         TM    IO+ACKEYSBR-ACKEYD,X'0F'                                         
         BZ    ME16                                                             
         B     ME10                LOOP TILL START OF NEXT BATCH                
         SPACE 2                                                                
ME12     XC    KEY,KEY             NO - GET FIRST                               
         MVI   ACBKCODE,X'0B'                                                   
         MVC   ACBKCOMP,COMPANY                                                 
         MVC   ACBKTYPE,FILTTYPE                                                
         GOTO1 HIGH                                                             
         B     ME16                                                             
         SPACE 2                                                                
ME14     GOTO1 SEQ                                                              
*                                                                               
ME16     CLC   KEYSAVE(2),KEY      BATCH FOR COMPANY                            
         BNE   ME24                                                             
         LA    R4,IO                                                            
         CLC   ACBKDATE,TODAY                                                   
         BNE   ME14                                                             
         CLI   FILTTYPE,0          FILTERS                                      
         BE    ME18                                                             
         CLC   FILTTYPE,ACBKTYPE   TYPE                                         
         BNE   ME14                                                             
         SPACE 2                                                                
ME18     CLI   FILTSTAT,0          STATUS                                       
         BE    ME22                                                             
         CLI   FILTSTAT,C'Y'                                                    
         BNE   ME20                                                             
         TM    IO+44,X'40'         OK                                           
         BO    ME14                                                             
         B     ME22                                                             
         SPACE 1                                                                
ME20     CLI   FILTSTAT,C'U'                                                    
         BE    ME21                                                             
         TM    IO+44,X'40'         NOT OK                                       
         BNO   ME14                                                             
         B     ME22                                                             
         SPACE 1                                                                
ME21     TM    IO+44,X'20'         UPDATED                                      
         BNO   ME14                                                             
         B     ME22                                                             
         SPACE 2                                                                
ME22     LA    R4,IO                                                            
         USING ACBKEYD,R4                                                       
         LA    R5,ACBKREF                                                       
         LA    R6,FILTFILT                                                      
         LA    R0,6                                                             
ME23     CLI   0(R6),X'41'         BATCH REF. FILTER                            
         BL    ME23B                                                            
         CLC   0(1,R6),0(R5)                                                    
         BNE   ME14                                                             
*                                                                               
ME23B    LA    R5,1(R5)                                                         
         LA    R6,1(R6)                                                         
         BCT   R0,ME23                                                          
*                                                                               
         TM    IO+ACKEYSBR-ACKEYD,X'0F'                                         
         BNZ   ME23C                                                            
         BAS   RE,FORMAT           NEW HEADER SO DISPLAY LAST                   
         CLC   AREA,SPACES                                                      
         BE    *+8                                                              
         BAS   RE,BUMP                                                          
         CP    TOTBAT,=P'12'                                                    
         BE    ME23D                                                            
ME23C    MVC   PREVKEY,KEY                                                      
         BAS   RE,DOBAT                                                         
         B     ME14                                                             
*                                                                               
ME23D    MVC   AREA,SPACES         TAKE A BLANK LINE                            
         BAS   RE,OPT                                                           
         BAS   RE,BUMP                                                          
         BAS   RE,TOTALS                                                        
         BAS   RE,OPT                                                           
         LA    R2,LOGRPTH                                                       
         MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(38),=C'BATCHES DISPLAYED - HIT ENTER FOR NEXT'           
         B     XIT                                                              
*                                                                               
ME24     BAS   RE,FORMAT                                                        
         CLC   AREA,SPACES                                                      
         BE    *+8                                                              
         BAS   RE,BUMP                                                          
         MVC   AREA,SPACES                                                      
         BAS   RE,OPT                                                           
         BAS   RE,BUMP                                                          
         BAS   RE,TOTALS                                                        
         LA    R3,13               MAX LINES                                    
         ZAP   DUB,TOTBAT                                                       
         CVB   RE,DUB                                                           
         SR    R3,RE               R3 = NO. OF REMAINING SLOTS                  
         BNZ   ME28                                                             
         BAS   RE,OPT                                                           
         B     ME29                                                             
*                                                                               
ME26     MVC   AREA,SPACES                                                      
*                                                                               
ME28     BAS   RE,OPT                                                           
         BAS   RE,BUMP                                                          
         BCT   R3,ME26                                                          
ME29     XC    PREVKEY,PREVKEY                                                  
         LA    R2,LOGINPH                                                       
         MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(35),=C'NO FURTHER BATCHES OF SELECTED TYPE'              
         B     XIT                                                              
         EJECT                                                                  
*              UTILITY ROUTINES                                                 
         SPACE 3                                                                
OPT      CLC   8(79,R2),AREA                                                    
         BCR   8,RE                                                             
         FOUT  (R2),AREA,79                                                     
         BR    RE                                                               
         SPACE 2                                                                
BUMP     SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
DOBAT    NTR1                                                                   
         LA    R4,IO                                                            
         MVC   BATTYP,ACBKTYPE-ACBKEYD(R4)                                      
         MVC   BATREF,ACBKREF-ACBKEYD(R4)                                       
         MVI   ELCODE,6            BATCH DESCRIPTION                            
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING ACBTCHD,R4                                                       
         MVC   BATNAME,ACBHNAME                                                 
         ZAP   BATITEM,ACBHITEM    THIS BATCH ITEMS                             
         ZAP   BATCASH,ACBHCASH                                                 
         MVC   BATSTAT,IO+44                                                    
         SR    R6,R6                                                            
*                                                                               
DOBAT2   ZIC   RF,1(R4)            DETAILS                                      
         AR    R4,RF                                                            
         CLI   0(R4),0                                                          
         BE    DOBAT4                                                           
         CLI   0(R4),X'08'         BINARY AMOUNT ELEMENT                        
         BE    DOBAT3                                                           
         CLI   0(R4),X'07'         PACKED                                       
         BNE   DOBAT2                                                           
*                                                                               
         USING ACPTEMD,R4                                                       
         OC    ACPTDA,ACPTDA                                                    
         BZ    DOBAT2                                                           
         CLI   BATTYP,45           FOR BATCH TYPE 45,                           
         BNE   *+12                                                             
         CLI   ACPTDA,X'80'        ONLY INCLUDE DEBITS IN TOTAL                 
         BNE   DOBAT2                                                           
         LA    R6,1(R6)                                                         
         AP    TOTITM,ACPTAMNT                                                  
         B     DOBAT2                                                           
         USING ACITEMD,R4                                                       
DOBAT3   OC    ACITDA,ACITDA                                                    
         BZ    DOBAT2                                                           
         CLI   BATTYP,45           FOR BATCH TYPE 45,                           
         BNE   *+12                                                             
         CLI   ACITDA,X'80'        ONLY INCLUDE DEBITS IN TOTAL                 
         BNE   DOBAT2                                                           
         LA    R6,1(R6)                                                         
         ICM   R7,15,ACITAMNT                                                   
         CVD   R7,DUB                                                           
         AP    TOTITM,DUB                                                       
         B     DOBAT2                                                           
DOBAT4   CVD   R6,DUB                                                           
         AP    BATDITEM,DUB        ITEMS ON SCREEN                              
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
FORMAT   NTR1                                                                   
         MVC   AREA,SPACES                                                      
         SR    R5,R5                                                            
         IC    R5,BATTYP                                                        
         LTR   R5,R5                                                            
         BZ    XIT                 NOTHING THERE                                
         BCTR  R5,0                                                             
         MH    R5,=H'15'                                                        
         LA    R5,ACINPT(R5)                                                    
         MVC   AREA(15),0(R5)                                                   
         MVC   AREA+16(6),BATREF                                                
         MVC   AREA+23(15),BATNAME                                              
         EDIT  (P2,BATITEM),(3,AREA+46)                                         
         EDIT  (P6,BATCASH),(13,AREA+49),2,MINUS=YES                            
         MVC   AREA+39(2),=C'OK'                                                
         TM    BATSTAT,X'40'                                                    
         BNO   FORM1                                                            
         MVC   AREA+39(6),=C'NOT-OK'                                            
         B     FORM4                                                            
FORM1    TM    BATSTAT,X'20'                                                    
         BNO   FORM4                                                            
         MVC   AREA+41(3),=C',UP'                                               
*                                                                               
FORM4    EDIT  (P2,BATDITEM),(3,AREA+63)                                        
         EDIT  TOTITM,(13,AREA+66),2,MINUS=YES                                  
         AP    TOTDCH,TOTITM       INCREMENT TOTAL ACCUMULATORS                 
         AP    TOTBIT,BATITEM                                                   
         AP    TOTBCH,BATCASH                                                   
         AP    TOTDIT,BATDITEM                                                  
         ZAP   TOTITM,=P'0'        AND CLEAR DOWN THESE FOR NEXT                
         ZAP   BATDITEM,=P'0'                                                   
         ZAP   BATITEM,=P'0'                                                    
         ZAP   BATCASH,=P'0'                                                    
         MVI   BATTYP,0                                                         
         BAS   RE,OPT                                                           
         AP    TOTBAT,=P'1'                                                     
         XIT1                                                                   
         GETEL (R4),DATADISP,ELCODE                                             
*                                                                               
TOTALS   NTR1                                                                   
         MVC   AREA,SPACES                                                      
         CP    TOTBAT,=P'2'                                                     
         BL    XIT                                                              
         MVC   AREA+16(21),=C'TOTALS FOR 12 BATCHES'                            
         EDIT  (P6,TOTBAT),(2,AREA+27)                                          
         EDIT  (P6,TOTBIT),(4,AREA+45)                                          
         EDIT  (P6,TOTBCH),(13,AREA+49),2,MINUS=YES                             
         EDIT  (P6,TOTDIT),(4,AREA+62)                                          
         EDIT  (P6,TOTDCH),(13,AREA+66),2,MINUS=YES                             
         B     XIT                                                              
         SPACE 2                                                                
XIT      XIT1  REGS=(R2)                                                        
         EJECT                                                                  
       ++INCLUDE ACGENINPT                                                      
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMEBD                                                       
PREVKEY  DS    CL32                                                             
AREA     DS    CL79                                                             
ELCODE   DS    CL1                                                              
FILTTYPE DS    CL1                                                              
FILTSTAT DS    CL1                                                              
FILTFILT DS    CL6                                                              
B45SW    DS    CL1                                                              
TOTBAT   DS    PL6                                                              
TOTBIT   DS    PL6                                                              
TOTBCH   DS    PL6                                                              
TOTDIT   DS    PL6                                                              
TOTDCH   DS    PL6                                                              
TOTITM   DS    PL6                                                              
BATDITEM DS    PL2                                                              
BATITEM  DS    PL2                                                              
BATCASH  DS    PL6                                                              
BATTYP   DS    C                                                                
BATSTAT  DS    C                                                                
BATNAME  DS    CL15                                                             
BATREF   DS    CL6                                                              
TODAY    DS    CL3                                                              
       ++INCLUDE ACLFMWORK                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACLFMEQU                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023ACLFM14   05/01/02'                                      
         END                                                                    
