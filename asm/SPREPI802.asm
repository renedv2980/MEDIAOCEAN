*          DATA SET SPREPI802  AT LEVEL 016 AS OF 04/07/15                      
*PROCESS USING(WARN(15))                                                        
*PHASE SPI802A                                                                  
         TITLE 'SPI802 - SPOT INVOICE RECORD PURGE/LIST'                        
*                                                                               
**********************************************************************          
*                                                                               
*        QOPT1- N MEANS FOR CLIENT=ALL REQUEST TO DO ALL CLIENTS                
*               TOGETHER RATHER THAN SEPARATELY                                 
*               ALSO MEANS NOT TO PRINT WRITE=YES/NO HEADLINE (WHY??)           
*        QOPT2- P MEANS PRINT DETAILS                                           
*        QOPT3- U MEANS UNCLOSE CLOSED OUT RECORDS                              
*        QOPT4- Y MEANS IGNORE PU PROFILES                                      
*        QOPT5- Y MEANS DRAFT MODE                                              
*                                                                               
**********************************************************************          
SPI802   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPI8                                                           
*                                                                               
         L     RA,0(R1)                                                         
         LA    R9,1(RA)                                                         
         LA    R9,4095(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         LA    RC,SPACEND                                                       
         USING WKD,RC                                                           
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,CLTFRST                                                     
         BE    CLTF                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
*                                                                               
RUNF     DS    0H                                                               
*                                                                               
         XC    CLTDISP,CLTDISP                                                  
         XCEFL SKIPTAB,1500      CLEAR SKIPPED CLIENTS TABLE                    
         LA    R0,SKIPTAB                                                       
         AHI   R0,SKIPTLQ                                                       
         ST    R0,ASKTEND                                                       
                                                                                
         B     EXIT                                                             
*                                                                               
         SPACE 2                                                                
*        REQUEST FIRST                                                          
*                                                                               
REQF     DS    0H                                                               
         MVC   SVRCW,RCWRITE                                                    
         CLI   QOPT5,C'Y'                                                       
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
*                                                                               
         MVI   CSW,C'C'            ONE CLT                                      
         CLC   QCLT,=C'ALL'                                                     
         BNE   REQF4                                                            
         MVI   CSW,C'N'            NO - ALL TOGETHER                            
         CLI   QOPT1,C'N'                                                       
         BE    *+8                                                              
         MVI   CSW,C'A'            ALL -EACH                                    
*                                                                               
REQF4    DS    0H                                                               
         XC    RTOTS,RTOTS                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
REQF5    DS    0H                  SET START-END DATES                          
         MVC   WORK(4),QSTART      NOTE- INVOICE KEY DATE IS START OF           
         MVC   WORK+4(2),=C'01'    BRDCAST MONTH SO THE 15TH OF THE             
         LA    R6,15               MONTH BEFOER AND AFTER ARE OK                
         LCR   R6,R6               AS FILTERS                                   
         GOTO1 ADDAY,DMCB,WORK,WORK,(R6)                                        
         GOTO1 DATCON,(R1),,(2,BQSTARTP)                                        
*                                                                               
         MVC   WORK(4),QEND                                                     
         MVC   WORK+4(2),=C'15'                                                 
         GOTO1 (RF),(R1),,(2,BQENDP)                                            
         CLI   CSW,C'N'                                                         
         BNE   EXIT                                                             
*                                                                               
*        BAS   RE,RDINV                                                         
         BAS   RE,RDINVNEW                                                      
         B     EXIT                                                             
         SPACE 2                                                                
*        CLIENT FIRST                                                           
*                                                                               
CLTF     DS    0H                                                               
         CLI   CSW,C'N'                                                         
         BE    EXIT                                                             
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         XC    CTOTS,CTOTS                                                      
*                                                                               
*        BAS   RE,RDINV                                                         
         BAS   RE,RDINVNEW                                                      
         B     EXIT                                                             
         SPACE 3                                                                
*        REQLAST                                                                
*                                                                               
REQL     DS    0H                                                               
         CLI   CSW,C'A'                                                         
*        BNE   EXIT                                                             
         BNE   REQLX                                                            
         MVC   P(9),=C'REQ TOTAL'                                               
         LA    R4,RTOTS                                                         
         BAS   RE,PRTOT                                                         
*                                                                               
         OC    SKIPTAB(3),SKIPTAB                                               
*        BZ    EXIT                  EXIT IF NO CLIENTS SKIPPED                 
         BZ    REQLX                 EXIT IF NO CLIENTS SKIPPED                 
*                                                                               
         LA    R2,SKIPTAB                                                       
         BAS   RE,RPRT                                                          
         MVC   P+12(20),=CL20'SKIPPED CLIENTS:'                                 
*                                                                               
REQL20   DS    0H                                                               
         C     R2,ASKTEND          PAST END OF SKIPTAB?                         
*        BNL   EXIT                                                             
         BNL   REQLX                                                            
         CLC   0(3,R2),=3X'00'                                                  
*        BE    EXIT                                                             
         BE    REQLX                                                            
         MVC   P+32(3),0(R2)       MOVE CLIENT TO PRINT LINE                    
         BAS   RE,RPRT                                                          
         LA    R2,3(R2)            ADVANCE TO NEXT CLIENT IN TABLE              
         B     REQL20                                                           
*        B     EXIT                                                             
*                                                                               
REQLX    DS    0H                                                               
         MVC   RCWRITE,SVRCW                                                    
         B     EXIT                                                             
*                                                                               
*                             INVOICE READER                                    
*                                                                               
*&&DO                                                                           
RDINV    NTR1                                                                   
         SPACE 2                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'0B'                                                        
         MVC   KEY+1(1),BAGYMD                                                  
         CLI   QOPT3,C'U'          IF UNCLOSE                                   
         BNE   RD3                                                              
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,X'FD'                                                   
*                                                                               
RD3      DS    0H                                                               
         GOTO1 HIGH                                                             
         B     RD4B                                                             
RD4      DS    0H                                                               
         GOTO1 SEQ                                                              
RD4B     DS    0H                                                               
         CLC   KEY(2),KEYSAVE      END OF AGY/MED                               
         BNE   RD40                                                             
         LA    R7,KEY                                                           
         USING INVKEYD,R7                                                       
*                                                                               
         CLI   CSW,C'N'                                                         
         BE    RD6                                                              
         CLC   INVKCLT,BCLT                                                     
         BNE   RD4                                                              
*                                                                               
RD6      DS    0H                                                               
         CLC   INVKDAT,BQSTARTP                                                 
         BL    RD4                                                              
*                                                                               
         CLC   INVKDAT,BQENDP                                                   
         BH    RD4                                                              
*                                                                               
         CLI   QOPT3,C'U'          TEST UNCLOSING                               
         BE    RD7                                                              
         OI    KEY+13,X'C0'        NO, CLOSE OUT RECORD                         
         GOTO1 WRITE                                                            
         B     RD7P                                                             
*                                                                               
RD7      DS    0H                  UNCLOSING                                    
         TM    KEY+13,X'C0'        TEST IF MARKED CLOSED                        
         BNO   RD4                 NO, SKIP                                     
         NI    KEY+13,X'3F'                                                     
         GOTO1 WRITE                                                            
         MVC   AREC,ADBUY                                                       
         GOTO1 GET                                                              
         L     RF,AREC                                                          
         TM    15(RF),X'C0'        TEST IS MARKED CLOSED OUT                    
         BNO   RD7P                                                             
         NI    15(RF),X'3F'                                                     
         GOTO1 PUT                                                              
*                                                                               
RD7P     DS    0H                                                               
         L     R1,CTOTS            COUNT RECORDS                                
         LA    R1,1(R1)                                                         
         ST    R1,CTOTS                                                         
*                                                                               
         CLI   KEY+9,0                                                          
         BNE   RD8                                                              
*                                                                               
         L     R1,CTOTS+4          COUNT ACTUAL INVOICES                        
         LA    R1,1(R1)                                                         
         ST    R1,CTOTS+4                                                       
*                                                                               
RD8      DS    0H                                                               
         CLI   QOPT2,C'P'                                                       
         BNE   RD30                                                             
*                                                                               
         MVC   P,SPACES                                                         
         LA    R2,P                                                             
         USING PLIND,R2                                                         
*                                                                               
         MVC   PLCLT,CLT                                                        
         CLI   CSW,C'N'                                                         
         BNE   RD22                                                             
*                                                                               
         GOTO1 CLUNPK,DMCB,INVKCLT,PLCLT                                        
*                                                                               
RD22     DS    0H                                                               
         GOTO1 MSUNPK,DMCB,INVKSTA-2,WORK,PLSTA                                 
         GOTO1 DATCON,DMCB,(2,INVKDAT),(5,PLDATE)                               
         CLI   INVKSEQ,0                                                        
         BE    RD24                                                             
         ZIC   R1,INVKSEQ                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLRNO,DUB                                                        
*                                                                               
RD24     DS    0H                                                               
         GOTO1 RPRT                                                             
*                                                                               
RD30     DS    0H                                                               
         B     RD4                                                              
*                                                                               
RD40     DS    0H                                                               
         LA    R4,CTOTS                                                         
         CLI   CSW,C'A'                                                         
         BNE   RD41                                                             
         OC    CTOTS,CTOTS         SKIP CLIENTS WITH NONE                       
         BZ    EXIT                                                             
RD41     DS    0H                                                               
         BAS   RE,PRTOT                                                         
         LA    R2,CTOTS                                                         
         LA    R3,RTOTS                                                         
         LA    R0,4                                                             
RD42     DS    0H                                                               
         L     RF,0(R2)                                                         
         A     RF,0(R3)                                                         
         ST    RF,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,RD42                                                          
*                                                                               
         XC    CTOTS,CTOTS                                                      
         B     EXIT                                                             
         SPACE 3                                                                
*&&                                                                             
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                        NEW  INVOICE READER                                    
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
RDINVNEW NTR1                                                                   
*                                                                               
         MVC   LASTDATE,BQENDP            END DATE                              
         XC    LASTDATE,=XL2'FFFF'        CONVERT DATE TO RECORD FORMAT         
         XC    LASTSTA,LASTSTA            ZERO OUT LAST STATION                 
*                                                                               
         XC    KEY32,KEY32                                                      
         MVC   KEY32(2),=X'0E03'                                                
         MVC   KEY32+2(1),BAGYMD                                                
         CLI   CSW,C'N'                                                         
         BE    *+10                                                             
         MVC   KEY32+3(2),BCLT                                                  
*                                                                               
         CLI   QOPT3,C'U'          IF UNCLOSE                                   
         BNE   RDN3                                                             
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,X'FD'                                                   
*                                                                               
RDN3     DS    0H                                                               
         MVC   KEY32SAV,KEY32                                                   
RDN3A    GOTO1 DATAMGR,DMCB,(DMINBTS,=C'DMRDHI'),=C'XSPDIR',KEY32,KEY32         
         B     RDN4B       SKIP OVER RDSEQ TO TERMINATION CHECKS                
RDN4     DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(DMINBTS,=C'DMRSEQ'),=C'XSPDIR',KEY32,KEY32         
RDN4B    DS    0H                                                               
         CLC   KEY32(3),KEY32SAV      END OF AGY/MED                            
         BNE   RDN40                                                            
*                                                                               
         LA    R7,KEY32                                                         
         USING SNVKEYD,R7                                                       
         CLI   CSW,C'N'                                                         
         BE    RDN5                                                             
         CLC   SNVKCLT,BCLT                                                     
         BNE   RDN40                                                            
*                                                                               
RDN5     DS    0H                                                               
         CLC   LASTSTA,KEY32+5     SAME STATION?                                
         BE    RDN6                IF YES, SKIP READHIGH                        
         MVC   LASTSTA,KEY32+5     SAVE LAST STATION                            
         MVC   KEY32+8(2),LASTDATE    MOVE END DATE INTO KEY                    
* SKIP RECORDS THAT WERE ADDED AFTER END DATE IN REQUEST CARD                   
         XC    KEY32+10(30),KEY32+10                                            
         B     RDN3A                                                            
*                                                                               
*DN4C    DS    0H                                                               
*        LA    R7,KEY32                                                         
*        USING SNVKEYD,R7                                                       
*                                                                               
*        CLI   CSW,C'N'                                                         
*        BE    RDN6                                                             
*        CLC   SNVKCLT,BCLT                                                     
*        BNE   RDN4                                                             
*                                                                               
RDN6     DS    0H                                                               
         MVC   TEMPDATE,SNVKMOS                                                 
         XC    TEMPDATE,=X'FFFF'                                                
         CLC   TEMPDATE,BQSTARTP                                                
         BL    RDN4                                                             
*                                                                               
         CLC   TEMPDATE,BQENDP                                                  
         BH    RDN3A                                                            
*                                                                               
* CHECK THE EXCLUDED CLIENTS                                                    
*                                                                               
         CLI   QOPT4,C'Y'          IGNORE PU PROFILES?                          
         BE    RDN6AA               YES                                         
         GOTO1 CLUNPK,DMCB,SNVKCLT,CLT3                                         
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0PU'    SPOT PURGE CLOSEOUT CONTROL                  
         MVC   WORK+4(2),QAGY                                                   
         MVC   WORK+6(1),QMED                                                   
*                                                                               
         MVC   WORK+7(3),CLT3                                                   
*                                                                               
         L     RF,ADCLT                                                         
         LA    RF,COFFICE-CLTHDR(RF)                                            
*        CLI   0(RF),C' '                                                       
*        BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),0(RF)                                                 
*                                                                               
         GOTO1 GETPROF,DMCB,(X'90',WORK),PUPROF,DATAMGR                         
         CLI   PUPROF+2,C'Y'       SKIP CLIENT?                                 
         BNE   RDN6AA              NO                                           
*                                                                               
* SAVE CLIENT IN THE TABLE                                                      
         LA    R1,SKIPTAB          TABLE OF SKIPPED CLIENTS                     
         L     R0,CLTDISP          DISPLACEMENT TO NEXT CLIENT                  
         AR    R1,R0                                                            
*                                                                               
         C     R1,ASKTEND                                                       
         BL    *+6                                                              
         DC    H'0'                TABLE OF CLIENTS FULL                        
*                                                                               
         MVC   0(3,R1),CLT3                                                     
         AHI   R0,3                                                             
         ST    R0,CLTDISP                                                       
*                                                                               
         MVC   SNVKSTA,=X'FFFFFF'    TURN ON ALL BITS IN STATION                
         B     RDN3A                 TO GET NEXT CLIENT ON READHI               
*                                                                               
RDN6AA   DS    0H                                                               
         CLI   QOPT3,C'U'          TEST UNCLOSING                               
         BE    RDN7                                                             
         OI    KEY32+32,X'C0'        NO, CLOSE OUT RECORD                       
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   RDN6A                                                            
         GOTO1 DATAMGR,DMCB,(0,=C'DMWRT'),=C'XSPDIR',KEY32,KEY32                
RDN6A    DS    0H                                                               
         B     RDN7P                                                            
*                                                                               
RDN7     DS    0H                  UNCLOSING                                    
         TM    KEY32+32,X'C0'        TEST IF MARKED CLOSED                      
         BNO   RDN4                 NO, SKIP                                    
         NI    KEY32+32,X'3F'                                                   
         CLI   RCWRITE,C'Y'                                                     
         BNE   RDN7A                                                            
         GOTO1 DATAMGR,DMCB,(0,=C'DMWRT'),=C'XSPDIR',KEY32,KEY32                
RDN7A    DS    0H                                                               
         MVC   AREC,ADBUY                                                       
         CLI   RCWRITE,C'Y'                                                     
         BNE   RDN7B                                                            
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'XSPFIL',KEY32+36,AREC,   X        
               WORK                                                             
RDN7B    DS    0H                                                               
         L     RF,AREC                                                          
         TM    15(RF),X'C0'        TEST IS MARKED CLOSED OUT                    
         BNO   RDN7P                                                            
         NI    15(RF),X'3F'                                                     
         CLI   RCWRITE,C'Y'                                                     
         BNE   RDN7C                                                            
         GOTO1 DATAMGR,DMCB,(0,=C'PUTREC'),=C'XSPFIL',KEY32+36,AREC,   X        
               WORK                                                             
RDN7C    DS    0H                                                               
*                                                                               
RDN7P    DS    0H                                                               
         L     R1,CTOTS            COUNT RECORDS                                
         LA    R1,1(R1)                                                         
         ST    R1,CTOTS                                                         
*                                                                               
         CLC   KEY32+24(6),=6X'FF'                                              
         BNE   RDN8                                                             
*                                                                               
         L     R1,CTOTS+4          COUNT ACTUAL INVOICES                        
         LA    R1,1(R1)                                                         
         ST    R1,CTOTS+4                                                       
*                                                                               
RDN8     DS    0H                                                               
         CLI   QOPT2,C'P'                                                       
         BNE   RDN30                                                            
*                                                                               
         MVC   P,SPACES                                                         
         LA    R2,P                                                             
         USING PLIND,R2                                                         
*                                                                               
         MVC   PLCLT,CLT                                                        
         CLI   CSW,C'N'                                                         
         BNE   RDN22                                                            
*                                                                               
         GOTO1 CLUNPK,DMCB,SNVKCLT,PLCLT                                        
*                                                                               
RDN22    DS    0H                                                               
         GOTO1 MSUNPK,DMCB,SNVKSTA-2,WORK,PLSTA                                 
         GOTO1 DATCON,DMCB,(2,TEMPDATE),(5,PLDATE)                              
*                                                                               
RDN24    DS    0H                                                               
         GOTO1 RPRT                                                             
*                                                                               
RDN30    DS    0H                                                               
         B     RDN4                                                             
*                                                                               
RDN40    DS    0H                                                               
         LA    R4,CTOTS                                                         
         CLI   CSW,C'A'                                                         
         BNE   RDN41                                                            
         OC    CTOTS,CTOTS         SKIP CLIENTS WITH NONE                       
         BZ    EXIT                                                             
RDN41    DS    0H                                                               
         BAS   RE,PRTOT                                                         
         LA    R2,CTOTS                                                         
         LA    R3,RTOTS                                                         
         LA    R0,4                                                             
RDN42    DS    0H                                                               
         L     RF,0(R2)                                                         
         A     RF,0(R3)                                                         
         ST    RF,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,RDN42                                                         
*                                                                               
         XC    CTOTS,CTOTS                                                      
         B     EXIT                                                             
*                                                                               
*                                                                               
PRTOT    NTR1                                                                   
         BAS   RE,RPRT                                                          
         MVC   P+12(09),=C'INVOICES='                                           
         EDIT  (B4,4(R4)),(8,P+21)                                              
         MVC   P2+12(08),=C'RECORDS='                                           
         EDIT  (B4,0(R4)),(8,P2+21)                                             
         BAS   RE,RPRT                                                          
*                                                                               
PRTOTX   DS    0H                                                               
         B     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
RPRT     NTR1                                                                   
         MVI   RCSUBPRG,0                                                       
         CLI   CSW,C'N'                                                         
         BE    *+8                                                              
         MVI   RCSUBPRG,10                                                      
*                                                                               
         CLI   QOPT1,C'N'                                                       
         BE    RP4                                                              
         MVC   HEAD5+61(09),=C'WRITE=YES'                                       
         CLI   RCWRITE,C'Y'                                                     
         BE    *+10                                                             
         MVC   HEAD5+61+6(3),=C'NO '                                            
*                                                                               
RP4      DS    0H                                                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
WKD      DSECT                                                                  
CTOTS    DS    XL16                                                             
RTOTS    DS    XL16                                                             
CSW      DS    X                                                                
TEMPDATE DS    XL2                                                              
LASTDATE DS    XL2                                                              
LASTSTA  DS    XL3                                                              
KEY32    DS    XL40                                                             
KEY32SAV DS    XL40                                                             
CLT3     DS    XL3                                                              
PUPROF   DS    CL16                                                             
CLTDISP  DS    F                                                                
SVRCW    DS    X                                                                
*                                                                               
ASKTEND  DS    A                                                                
SKIPTAB  DS    CL(SKIPTLQ)                                                      
SKIPTLQ  EQU   1500                500 CLIENTS                                  
*                                                                               
         SPACE 3                                                                
*                   INVOICE ITEM ELEMENT                                        
         SPACE 3                                                                
INVELEMD DSECT                                                                  
INVELEM  DS    0CL13                                                            
         DS    X'B1' .   B         ELEMENT CODE                                 
         DS    X'0D' .   B         ELEMENT LENGTH                               
INVDAT   DS    CL2 .     B         DATE                                         
INVTIM   DS    CL2 .     B         START TIME - MILITARY BUT TIMES              
*                                    BEFORE 0600 HAVE 2400 ADDED                
INVLEN   DS    CL1 .     B         SPOT LENGTH                                  
INVCOST  DS    CL3 .     B         COST                                         
INVSTAT  DS    CL1 .     B         STATUS                                       
INVPRD   DS    CL1 .     B         PRODUCT CODE                                 
INVPRD2  DS    CL1 .     B         2ND PROD IF PIGGYBACK                        
*                  INVOICE KEY                                                  
         SPACE 3                                                                
INVKEYD  DSECT                                                                  
INVKEY   DS    0CL13                                                            
         DS    CL1 .                                                            
INVKAM   DS    CL1 .     B         AGENCY/MEDIA                                 
INVKSTA  DS    CL3 .     B         STATION (5 BIT ALPHA CODE)                   
INVKCLT  DS    CL2 .     A         CLIENT                                       
INVKDAT  DS    CL2 .     B         DATE (MONDAY OF WEEK)                        
INVKSEQ  DS    CL1 .     B         RECORD SEQUENCE NO.                          
         DS    CL3 .               SPARE                                        
PLIND    DSECT                                                                  
PLIN     DS    0CL132                                                           
         DS    CL3                                                              
PLCLT    DS    CL3                                                              
         DS    CL3                                                              
PLSTA    DS    CL7                                                              
         DS    CL3                                                              
PLDATE   DS    CL8                                                              
         DS    CL3                                                              
PLRNO    DS    CL3                                                              
         DS    CL99                                                             
         SPACE 3                                                                
         PRINT OFF                                                              
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016SPREPI802 04/07/15'                                      
         END                                                                    
