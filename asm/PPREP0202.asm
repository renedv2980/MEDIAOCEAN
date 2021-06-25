*          DATA SET PPREP0202  AT LEVEL 168 AS OF 05/01/02                      
*PHASE PP0202A,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PP0202 - PRTFIX PROGRAM'                                        
*                                                                               
*        THIS PROGRAM WILL READ PUBS AND CHECK THE PUBGST CODE                  
*        TO THAT OF PBUYRECS FOR THAT PUB                                       
*                                                                               
*        QOPT5     Y= TEST RUN (DON'T MARK FILE)                                
*                                                                               
PP0202   CSECT                                                                  
         NMOD1 0,PP0202                                                         
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
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   INCNT,=P'0'                                                      
         ZAP   ERRCNT,=P'0'                                                     
         ZAP   NET,=P'0'                                                        
         ZAP   CD,=P'0'                                                         
         ZAP   MYTAX,=P'0'                                                      
         ZAP   BASIS,=P'0'                                                      
         ZAP   BTAX,=P'0'                                                       
         ZAP   BTAXPD,=P'0'                                                     
         ZAP   BTAXBL,=P'0'                                                     
         XC    LASTPUB,LASTPUB                                                  
         LA    R0,BUFREC                                                        
         ST    R0,BUFFIO                                                        
         L     R0,=A(BUFFALOC)                                                  
         ST    R0,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',BUFFBUFF                                    
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         OC    LASTPUB,LASTPUB                                                  
         BZ    PROC3                                                            
         CLI   PUBSW,1                                                          
         BNE   PROC3                                                            
         BAS   RE,PUBEND                                                        
*                                                                               
PROC3    ZAP   PUBNET,=P'0'                                                     
         ZAP   PUBCASH,=P'0'                                                    
         ZAP   PUBMYTAX,=P'0'                                                   
         ZAP   PUBBASIS,=P'0'                                                   
         ZAP   PUBGSTT,=P'0'                                                    
         ZAP   PUBGSTTP,=P'0'                                                   
         MVC   LASTPUB,PUBKPUB                                                  
         MVI   PUBSW,0                                                          
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
*                                                                               
         MVC   CHKGST(1),PUBGST                                                 
         CLI   CHKGST,0                                                         
         BNE   *+8                                                              
         MVI   CHKGST,C'S'                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY      STARTING AGY/MEDIA                           
         MVI   KEY+3,X'21'         CLIENTS                                      
         MVC   KEY+4(3),QCLIENT    START AT CLIENT                              
         MVC   KEY+7(6),PUBKPUB                                                 
*                                                                               
AGYC2    GOTO1 HIGH                                                             
         B     AGYC4                                                            
*                                                                               
AGYC3    DS    0H                                                               
         GOTO1 SEQ                                                              
AGYC4    DS    0H                                                               
         CLC   KEY(4),KEYSAVE        AGY/MED/RECORD                             
         BNE   EXIT                                                             
         CLC   KEY+7(6),PUBKPUB      CHECK RIGHT PUB                            
         BH    NEXTCLT                SKIP TO NEXT CLIENT                       
         BE    AGYC6                                                            
         MVC   KEY+7(6),PUBKPUB       SEARCH FOR THIS PUB                       
         XC    KEY+13(12),KEY+13                                                
         B     AGYC2                                                            
*                                                                               
AGYC6    LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         AP    INCNT,=P'1'                                                      
*                                                                               
         TM    PBDCNDA,X'80'           SEE IF CANADIAN                          
         BZ    AGYC3                                                            
*                                                                               
*                                                                               
         CLC   PBDLIST,=C'   '    CHECK FOR LIST CODE                           
         BNH   AGYC3               NO - THEN SKIP                               
*                                                                               
         CLC   PBDGST,CHKGST                                                    
         BE    AGYC3                                                            
         AP    ERRCNT,=P'1'                                                     
         MVI   PUBSW,1                                                          
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY+27,PSECOND+122,4,=C'N'                           
         MVC   PSECOND+5(2),=C'L='                                              
         MVC   PSECOND+7(3),PBDLIST                                             
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(5,PSECOND+13)                          
         CLI   PBUYKLIN,1                                                       
         BE    AGYC7                                                            
         MVI   PSECOND+21,C'-'                                                  
         ZIC   R0,PBUYKLIN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PSECOND+22(2),DUB                                                
*                                                                               
AGYC7    DS    0H                                                               
         MVC   P+1(3),PBUYKCLT                                                  
         MVC   P+5(3),PBUYKPRD                                                  
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+10(3),DUB                                                      
         GOTO1 PUBEDIT,DMCB,PBUYKPUB,P+15                                       
         XC    GVALUES(28),GVALUES                                              
         GOTO1 GETINS,DMCB,PBUYREC,GROSS,(C'Y',PBUYREC+7),0,=C'GST'             
         BAS   RE,DODOLS                                                        
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'25'                                                     
AGYC8    BAS   RE,NEXTEL                                                        
         BNE   AGYC9                                                            
         OC    2(3,R2),2(R2)                                                    
         BZ    AGYC8                                                            
         MVC   PSECOND+30(10),=C'DATE PAID='                                    
         GOTO1 DATCON,DMCB,(3,2(R2)),(5,PSECOND+41)                             
         MVC   PSECOND+50(13),=C'CONTROL DATE='                                 
         GOTO1 DATCON,DMCB,(3,5(R2)),(5,PSECOND+64)                             
         BAS   RE,RPRT                                                          
         B     AGYC8                                                            
AGYC9    DS    0H                                                               
         BAS   RE,RPRT                                                          
         B     AGYC3             SEQ READ                                       
*                                                                               
NEXTCLT  DS    0H                                                               
         ZIC   R1,KEY+6                                                         
         LA    R1,1(R1)                                                         
         STC   R1,KEY+6                                                         
         MVC   KEY+7(6),PUBKPUB                                                 
         MVI   KEY+3,X'21'                                                      
         XC    KEY+13(12),KEY+13                                                
         B     AGYC2                                                            
*                                                                               
AGYC10   DS    0H                                                               
         B     NEXTAGY                                                          
*                                                                               
FPROD    MVI   KEY+3,X'40'                                                      
         XC    KEY+4(28),KEY+4                                                  
         B     AGYC2                                                            
*                                                                               
NEXTAGY  CLI   KEY,X'FF'          END OF FILE                                   
         BE    EXIT                                                             
         ZIC   R1,KEY+2                                                         
         LA    R1,1(R1)                                                         
         STC   R1,KEY+2                                                         
         B     FPROD                                                            
         EJECT                                                                  
*                                                                               
DODOLS   NTR1                                                                   
         L      R7,DMCB+16                                                      
         USING  GVALUES,R7                                                      
         XC     BUFREC,BUFREC                                                   
         ZAP    BUFNET,=P'0'                                                    
         ZAP    BUFCD,=P'0'                                                     
         ZAP    BUFTAX,=P'0'                                                    
         ZAP    BUFBASIS,=P'0'                                                  
         ZAP    BUFGST,=P'0'                                                    
         ZAP    BUFGSTPD,=P'0'                                                  
         MVI    BUFTYPE,X'01'                                                   
         MVC    BUFCLT,PBUYKCLT                                                 
         MVC    BUFPUB,PBUYKPUB                                                 
*                                                                               
         L      R4,PGROSS                                                       
         S      R4,PAGYCOM                                                      
         ST     R4,FULL                                                         
         L      R0,FULL                                                         
         CVD    R0,DUB                                                          
         AP     BUFNET,DUB                                                      
         AP     NET,DUB                                                         
         AP     PUBNET,DUB                                                      
         EDIT  (B4,FULL),(14,P+30),2,COMMAS=YES,FLOAT=-                         
         EDIT  (B4,PCSHDSC),(14,P+45),2,COMMAS=YES,FLOAT=-                      
         EDIT  (B4,TAX),(14,P+60),2,COMMAS=YES,FLOAT=-                          
         S     R4,TAX                                                           
         ST    R4,FULL                                                          
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         AP    BUFBASIS,DUB                                                     
         AP    BASIS,DUB                                                        
         AP    PUBBASIS,DUB                                                     
         EDIT  (B4,FULL),(14,P+75),2,COMMAS=YES,FLOAT=-                         
         EDIT  (B4,GSTTAX),(14,P+90),2,COMMAS=YES,FLOAT=-                       
         EDIT  (B4,GSTTAXPD),(14,P+105),2,COMMAS=YES,FLOAT=-                    
         EDIT  (B2,GSTPCT),(8,P+122),4                                          
         MVC   P+131(1),GSTCODE                                                 
         L     R0,PCSHDSC                                                       
         CVD   R0,DUB                                                           
         AP    BUFCD,DUB                                                        
         AP    CD,DUB                                                           
         AP    PUBCASH,DUB                                                      
         L     R0,TAX                                                           
         CVD   R0,DUB                                                           
         AP    BUFTAX,DUB                                                       
         AP    MYTAX,DUB                                                        
         AP    PUBMYTAX,DUB                                                     
*                                                                               
         L     R0,GSTTAX                                                        
         CVD   R0,DUB                                                           
         AP    BUFGST,DUB                                                       
         AP    BTAX,DUB                                                         
         AP    PUBGSTT,DUB                                                      
         L     R0,GSTTAXPD                                                      
         CVD   R0,DUB                                                           
         AP    BUFGSTPD,DUB                                                     
         AP    BTAXPD,DUB                                                       
         AP    PUBGSTTP,DUB                                                     
         L     R0,GSTTAXBL                                                      
         CVD   R0,DUB                                                           
         AP    BTAXBL,DUB                                                       
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVC   BUFPUB,=6X'FF'                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVC   BUFCLT,=3X'FF'                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         XIT1                                                                   
         DROP  R7                                                               
         EJECT                                                                  
PUBEND   NTR1                                                                   
         MVC   P+1(10),=C'PUB TOTALS'                                           
         EDIT  (P8,PUBNET),(14,P+30),2,COMMAS=YES,FLOAT=-                       
         EDIT  (P8,PUBCASH),(14,P+45),2,COMMAS=YES,FLOAT=-                      
         EDIT  (P8,PUBMYTAX),(14,P+60),2,COMMAS=YES,FLOAT=-                     
         EDIT  (P8,PUBBASIS),(14,P+75),2,COMMAS=YES,FLOAT=-                     
         EDIT  (P8,PUBGSTT),(14,P+90),2,COMMAS=YES,FLOAT=-                      
         EDIT  (P8,PUBGSTTP),(14,P+105),2,COMMAS=YES,FLOAT=-                    
         BAS   RE,RPRT                                                          
         B     PUBENDX                                                          
PUBENDX  XIT1                                                                   
*                                                                               
RUNL     DS    0H                                                               
         BAS   RE,PUBEND                                                        
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         LA    R4,COUNTS                                                        
RUNL5    CLI   0(R4),X'FF'                                                      
         BE    RUNL10                                                           
         MVC   P(15),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,23(R4)                                                        
         B     RUNL5                                                            
*                                                                               
RUNL10   MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,20                                                      
         XC    BUFREC,BUFREC                                                    
         MVI   BUFTYPE,X'01'                                                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',BUFFBUFF,BUFREC,0                          
         B     RUNL20                                                           
RUNL15   GOTO1 BUFFALO,DMCB,=C'SEQ',(TYPE,BUFFBUFF),BUFREC,0                    
*                                                                               
RUNL20   CLI   DMCB+8,X'80'     END                                             
         BE    RUNLX                                                            
         MVC   P+9(3),BUFCLT                                                    
         CLC   BUFCLT,=3X'FF'                                                   
         BNE   *+10                                                             
         MVC   P+9(3),=C'ALL'                                                   
         MVC   P+14(3),=C'ALL'                                                  
         CLC   BUFPUB,=6X'FF'                                                   
         BE    RUNL30                                                           
         GOTO1 PUBEDIT,DMCB,BUFPUB,P+14                                         
RUNL30   EDIT  (P8,BUFNET),(14,P+30),2,COMMAS=YES,FLOAT=-                       
         EDIT  (P8,BUFCD),(14,P+45),2,COMMAS=YES,FLOAT=-                        
         EDIT  (P8,BUFTAX),(14,P+60),2,COMMAS=YES,FLOAT=-                       
         EDIT  (P8,BUFBASIS),(14,P+75),2,COMMAS=YES,FLOAT=-                     
         EDIT  (P8,BUFGST),(14,P+90),2,COMMAS=YES,FLOAT=-                       
         EDIT  (P8,BUFGSTPD),(14,P+105),2,COMMAS=YES,FLOAT=-                    
         GOTO1 REPORT                                                           
         B     RUNL15                                                           
*                                                                               
RUNLX    DS    0H                                                               
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                             LINK TO REPORT                                    
         SPACE 2                                                                
         DS    0F                                                               
RPRT     NTR1                                                                   
         SPACE 2                                                                
         MVI   RCSUBPRG,10                                                      
         MVC   HEAD5+62(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+68(3),=C'YES'                                              
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                             GET FIRST/NEXT PUB                                
NXTPUB   NTR1                                                                   
         SPACE 2                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         MVC   KEY+1(6),LASTPUB                                                 
         IC    R1,KEY+6                                                         
         LA    R1,1(R1)                                                         
         STC   R1,KEY+6                                                         
         MVC   KEY+7(2),PAGYKAGY                                                
         GOTO1 HIGHPUB                                                          
         B     NP2B                                                             
*                                                                               
NP2      DS    0H                                                               
         GOTO1 SEQPUB                                                           
NP2B     DS    0H                                                               
         MVI   PUBKMED,X'FF'       SET EOF                                      
         CLC   KEY(1),KEYSAVE                                                   
         BNE   NPX                                                              
         CLC   KEY+7(2),KEYSAVE+7                                               
         BNE   NP2                                                              
         CLI   KEY+9,X'81'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LASTPUB,KEY+1                                                    
         GOTO1 GETNAME                                                          
         L     RF,ALTLREC                                                       
         XC    0(50,RF),0(RF)                                                   
         GOTO1 SEQPUB                                                           
         CLC   KEY(9),PUBKEY                                                    
         BNE   NPX                                                              
         GOTO1 GETLTL                                                           
*                                                                               
NPX      DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
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
         SPACE 3                                                                
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
         LA    R2,220                                                           
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
         DC    CL15'RECORDS READ'                                               
ERRCNT   DS    PL8                                                              
         DC    CL15'GST MISMATCH'                                               
NET      DS    PL8                                                              
         DC    CL15'NET PAID'                                                   
CD       DS    PL8                                                              
         DC    CL15'CD PAID'                                                    
MYTAX    DS    PL8                                                              
         DC    CL15'TAX PAID'                                                   
BASIS    DS    PL8                                                              
         DC    CL15'GST BASIS'                                                  
BTAX     DS    PL8                                                              
         DC    CL15'GST TAX'                                                    
BTAXPD   DS    PL8                                                              
         DC    CL15'GST TAX-PD'                                                 
BTAXBL   DS    PL8                                                              
         DC    CL15'GST TAX-BL'                                                 
         DC    X'FF'                                                            
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
         BUFF  LINES=2000,ROWS=1,COLUMNS=6,FLAVOR=PACKED,KEYLIST=(10,A)X        
               ,COMMENT=10                                                      
         SPACE 2                                                                
PP02WRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
FRSTSW   DS    XL1                                                              
TYPE     DS    XL1                                                              
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
BUFFBUFF DS    A                                                                
BUFFIO   DS    A                                                                
ITOT     DS    F                                                                
SKEY     DS    CL64                                                             
*                                                                               
BUFREC   DS    0CL68                                                            
BUFKEY   DS    0CL10                                                            
BUFTYPE  DS    CL1                                                              
BUFCLT   DS    CL3                                                              
BUFPUB   DS    CL6                                                              
BUFCOM   DS    CL10                                                             
*                                                                               
BUFNET   DS    PL8                                                              
BUFCD    DS    PL8                                                              
BUFTAX   DS    PL8                                                              
BUFBASIS DS    PL8                                                              
BUFGST   DS    PL8                                                              
BUFGSTPD DS    PL8                                                              
*                                                                               
*                                                                               
       ++INCLUDE GVALUES                                                        
*                                                                               
       ++INCLUDE PBILPROF                                                       
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE DDBUFFALO                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'168PPREP0202 05/01/02'                                      
         END                                                                    
