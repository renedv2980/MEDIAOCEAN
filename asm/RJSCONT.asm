*          DATA SET RJSCONT    AT LEVEL 003 AS OF 05/01/02                      
*          DATA SET PPREP0202  AT LEVEL 113 AS OF 06/27/89                      
*PHASE PP0202,+0,NOAUTO                                                         
*INCLUDE MININAM                                                                
         TITLE 'PP0202 - PRTFIX PROGRAM'                                        
         PRINT NOGEN                                                            
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
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   MISCOM,=P'0'    MISSING COMMENTS                                 
         ZAP   INCNT,=P'0'                                                      
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
*                                                                               
         OI    DMINBTS,X'08'      PASS DELETED REC                              
         XC    KEY,KEY                                                          
         MVC   KEY(3),QOPT1        AGENCY/MEDIA                                 
         MVI   KEY+3,X'20'         BUY                                          
         CLC   QOPT1+3(3),=C'ALL'                                               
         BE    AGYC2                                                            
         MVC   KEY+4(3),QOPT1+3    START AT CLIENT                              
*                                                                               
AGYC2    GOTO1 HIGH                                                             
         B     AGYC4                                                            
*                                                                               
AGYC3    DS    0H                                                               
         GOTO1 SEQ                                                              
AGYC4    DS    0H                                                               
         CLI   KEY,255                                                          
         BE    EXIT                                                             
         CLC   KEY(2),=C'ZZ'                                                    
         BE    EXIT                                                             
         CLI   KEY+3,X'21'                                                      
         BE    BUMPAGY                                                          
         CLI   KEY+3,X'20'                                                      
         BH    BUMPAGY           NO MORE BUYS FOR AGY                           
         BL    FORCEREC                                                         
         CLI   KEY+2,C'N'                                                       
         BL    BUMPMED                                                          
         BH    BUMPAGY                                                          
         B     AGYC6                                                            
FORCEREC MVI   KEY+3,X'20'                                                      
         B     BUMPMED                                                          
BUMPMED  MVI   KEY+2,C'N'                                                       
         MVI   KEY+3,X'20'                                                      
         XC    KEY+7(18),KEY+7                                                  
         MVI   KEY+4,1                                                          
         B     AGYC2                                                            
BUMPAGY  ZIC   RE,KEY+1           ONLY MATCH AGENCY/MEDIA                       
         LA    RE,1(RE)           DONE                                          
         STC   RE,KEY+1                                                         
         MVI   KEY+2,C'N'                                                       
         MVI   KEY+3,X'20'                                                      
         XC    KEY+4(20),KEY+4                                                  
         B     AGYC2                                                            
AGYC6    DS    0H                                                               
         TM    KEY+25,X'80'                                                     
         BO    AGYC3               DELETED / BYPASS                             
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         AP    INCNT,=P'1'                                                      
         L     R6,AREC                                                          
         BAS   RE,GETEL                                                         
         BNE   AGYC3                                                            
         B     CLCR6                                                            
LOOP68   BAS   RE,NEXTEL                                                        
         BNE   AGYC3                                                            
CLCR6    CLC   0(2,R6),=X'6802'                                                 
         BNE   LOOP68                                                           
*                                                                               
         AP    MISCOM,=P'1'                                                     
         GOTO1 HEXOUT,DMCB,KEY,P+4,25 =C'N'                                     
         GOTO1 HEXOUT,DMCB,KEY+27,P+60,8,=C'N'                                  
*        GOTO1 HEXOUT,DMCB,KEYSAVE,PSECOND+30,20,=C'N'                          
         BAS   RE,RPRT                                                          
         GOTO1 WRT                                                              
         B     LOOP68                                                           
*                                                                               
         GETEL (R6),33,ELCODE                                                   
*                                                                               
ELCODE   DC    X'68'                                                            
*                                                                               
USERP    DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QOPT1                                                     
         MVI   KEY+3,X'30'         USERP                                        
*                                                                               
USER2    GOTO1 HIGH                                                             
         B     USER4                                                            
*                                                                               
USER3    DS    0H                                                               
         GOTO1 SEQ                                                              
USER4    DS    0H                                                               
         CLI   KEY+3,X'30'                                                      
         BNE   EXIT                                                             
         CLC   KEY(3),QOPT1       ONLY MATCH AGENCY/MEDIA                       
         BNE   EXIT               DONE                                          
*                                                                               
*                                                                               
*                                                                               
USER6    DS    0H                                                               
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         AP    INCNT,=P'1'                                                      
         MVC   SKEY,KEY                                                         
         MVI   DDSBLD,C'N'                                                      
         MVI   ERRSW,C'N'                                                       
         MVI   KEYPSW,C'N'                                                      
*                                                                               
         LA    R2,PCLTREC+33                                                    
         MVI   ELCODE,X'15'           CHK FOR BILLING ON PRINTPAK               
USER5    BAS   RE,NEXTEL                                                        
         BNE   USER3                                                            
*                                                                               
         MVC   SKEY,KEY                                                         
         LA    R5,2                                                             
         LA    R6,5(R2)                                                         
USERP5C  OC    0(6,R6),0(R6)                                                    
         BZ    USERP9                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(3),PCLTREC   AGY/MED                                         
         MVI   KEY+3,X'40'                                                      
         MVC   KEY+4(6),0(R6)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    USERP9                                                           
         AP    MISCOM,=P'1'                                                     
*                                                                               
         GOTO1 HEXOUT,DMCB,PCLTKEY,P+4,25 =C'N'                                 
         GOTO1 HEXOUT,DMCB,0(R2),P+60,23,=C'N'                                  
         GOTO1 HEXOUT,DMCB,KEYSAVE,PSECOND+30,20,=C'N'                          
         MVC   PSECOND+5(20),KEYSAVE                                            
         BAS   RE,RPRT                                                          
USERP9   LA    R6,6(R6)                                                         
         BCT   R5,USERP5C                                                       
*                                                                               
         MVC   KEY,SKEY                                                         
         GOTO1 HIGH                                                             
         B     USER3                                                            
**************   END OF MY CODE *****************                               
*                                                                               
         CLC   2(4,R2),=C'COM='       CHK FOR STANDARD COMMENT                  
         BNE   AGYC5                                                            
*                                                                               
         MVI   ERRSW,C'Y'                                                       
         B     GETCOM                                                           
*                                                                               
AGYCX    CLI   ERRSW,C'Y'                                                       
         BNE   AGYC3                                                            
         MVC   KEY,SKEY                                                         
         GOTO1 HIGH                     RESTORE SEQ READ                        
         B     AGYC3                                                            
*                                                                               
*          DATA SET PP72COM    AT LEVEL 003 AS OF 12/14/81                      
GETCOM   DS    0H                                                               
         LA    R3,1(R2)                                                         
STDC2    DS    0H                                                               
         CLC   1(4,R3),=C'COM='                                                 
         BNE   STDCOMX                                                          
         LA    R3,5(R3)                                                         
         LR    R4,R3                                                            
STDC4    DS    0H                                                               
         CLI   0(R4),C','                                                       
         BE    STDC6                                                            
         CLI   0(R4),0             EOR                                          
         BE    STDC6                                                            
         CLI   0(R4),X'66'         NEXT ELEM                                    
         BE    STDC6                                                            
         LA    R4,1(R4)                                                         
         B     STDC4                                                            
STDC6    DS    0H                                                               
         SR    R4,R3                                                            
         BNP   STDCERX                                                          
         CH    R4,=H'6'                                                         
         BH    STDCERX                                                          
         MVC   WORK(12),SPACES                                                  
         LA    R7,WORK+6                                                        
         SR    R7,R4                                                            
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R3)       COMM NO RIGHT ALIGN IN WORK                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PJOBREC   AGY/MED                                         
         MVI   KEY+3,X'40'                                                      
         MVC   KEY+4(6),WORK                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    FINDCX                                                           
         AP    MISCOM,=P'1'                                                     
*                                                                               
         GOTO1 HEXOUT,DMCB,PJOBKEY,P+4,25 =C'N'                                 
         GOTO1 HEXOUT,DMCB,0(R2),P+60,23,=C'N'                                  
         GOTO1 HEXOUT,DMCB,KEYSAVE,PSECOND+10,20,=C'N'                          
         BAS   RE,RPRT                                                          
FINDCX   LA    R3,1(R3,R4)                                                      
         CLI   0(R3),0           END OF REC                                     
         BE    STDCOMX                                                          
         B     STDC2                                                            
*                                                                               
STDCOMX  B     AGYC5                                                            
*                                                                               
STDCERX  DC    H'0'                                                             
*                                                                               
AGYC7    DS    0H                                                               
         LA    R4,=AL2(6,3)     CHK BILLABLE DATE -6,+3                         
         LA    R5,PBDBDATE                                                      
         LA    R6,BDCHG                                                         
         MVI   DATETYP,C'B'                                                     
         BAS   RE,RTSUBX5                                                       
*                                                                               
         LA    R4,=AL2(5,5)     CHK PAYABLE DATE -5,+5                          
         LA    R5,PBDPDATE                                                      
         LA    R6,PDCHG                                                         
         MVI   DATETYP,C'P'                                                     
         BAS   RE,RTSUBX5                                                       
*                                                                               
         CLI   ERRSW,C'N'                                                       
         BE    AGYC3           NO ERROR SO SKIP                                 
         CLI   DDSBLD,C'Y'     DDS BILLED - DON'T MARK BUY                      
         BNE   AGYC9                                                            
         AP    DDSBD,=P'1'                                                      
         B     AGYC3                                                            
*                                                                               
AGYC9    DS    0H                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   AGYC3                                                            
         GOTO1 PUTPRT                                                           
         B     AGYC3           GO DO NEXT BUY                                   
*                                                                               
*          DATA SET PPBUY03    AT LEVEL 135 AS OF 10/06/88                      
         SPACE 2                                                                
RTSUBX5  DS    0H                                                               
         OC    0(3,R5),0(R5)                                                    
         BZR   RE                                                               
         SR    RF,RF                                                            
         MVC   WORK(2),PBUYKDAT                                                 
         IC    RF,WORK+1                                                        
         SH    RF,0(R4)                                                         
         STC   RF,WORK+1                                                        
         BP    RTSUBX5B                                                         
         AH    RF,=H'12'                                                        
         STC   RF,WORK+1                                                        
         IC    RF,WORK                                                          
         BCTR  RF,R0                                                            
         STC   RF,WORK                                                          
RTSUBX5B DS    0H                                                               
         CLC   WORK(2),0(R5)                                                    
         BH    AGYCERR             DATE BELOW LIMIT                             
*                                                                               
         MVC   WORK(2),PBUYKDAT                                                 
         IC    RF,WORK+1                                                        
         AH    RF,2(R4)                                                         
         STC   RF,WORK+1                                                        
         CLI   WORK+1,12                                                        
         BNH   RTSUBX5D                                                         
         SH    RF,=H'12'                                                        
         STC   RF,WORK+1                                                        
         IC    RF,WORK                                                          
         LA    RF,1(RF)                                                         
         STC   RF,WORK                                                          
RTSUBX5D DS    0H                                                               
         CLC   WORK(2),0(R5)                                                    
         BL    AGYCERR             DATE ABOVE LIMIT                             
         BR    RE                                                               
         EJECT                                                                  
         DS    F                                                                
AGYCERR  ST    RE,AGYCERR-4                                                     
         MVI   ERRSW,C'Y'                                                       
         AP    0(8,R6),=P'1'                                                    
         MVC   OLDDATE,0(R5)                                                    
         MVC   0(3,R5),PBUYKDAT        USE INSERTION DATE                       
         CLI   KEYPSW,C'Y'                                                      
         BE    AGYCE5                                                           
         GOTO1 HEXOUT,DMCB,PBUYREC,P+5,40,=C'N'                                 
         GOTO1 HEXOUT,DMCB,KEY+27,P+95,4,=C'N'                                  
         MVI   KEYPSW,C'Y'                                                      
         BAS   RE,RPRT                                                          
*                                                                               
AGYCE5   MVC   P+1(1),DDSBLD                                                    
         MVC   P+3(1),DATETYP                                                   
         MVC   P+5(8),=C'OLD DATE'                                              
         GOTO1 HEXOUT,DMCB,OLDDATE,P+15,3,=C'N'                                 
         MVC   P+30(8),=C'NEW DATE'                                             
         GOTO1 HEXOUT,DMCB,0(R5),P+40,3,=C'N'                                   
         BAS   RE,RPRT                                                          
         L     RE,AGYCERR-4                                                     
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
COUNTS   DS    0C                                                               
INCNT    DS    PL8                                                              
         DC    CL15'BUY REC READ IN  '                                          
MISCOM   DS    PL8                                                              
         DC    CL15'BAD 6802 IN BUY'                                            
         DC    X'FF'                                                            
*                                                                               
CNT24    DS    PL4                                                              
CNT70    DS    PL4                                                              
OLDDATE  DS    CL3                                                              
ERRSW    DS    CL1                                                              
DDSBLD   DS    CL1                                                              
DATETYP  DS    CL1                                                              
KEYPSW   DS    CL1                                                              
*                                                                               
OUTSPC   DS    CL40                                                             
************************************* OLD CODE                                  
         LA    R7,PBUYREC                                                       
         ST    R7,AREC                                                          
         GOTO1 GETPRT                                                           
         GOTO1 GETINS,DMCB,PBUYREC,GROSS,PBUYKPRD                               
         OC    PGROSS,PGROSS                                                    
         BZ    PROC3                                                            
         TM    PBUYREC+27,X'80'       SEE IF DELETED AND PAID                   
         BZ    PROC3                                                            
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY,P,33,=C'N'                                       
         BAS   RE,RPRT                                                          
         B     PROC3                                                            
************************ END OF PROGRAM                                         
         CLC   KEY(4),KEYSAVE                                                   
         BNE   EXIT                                                             
*                                                                               
         MVC   WORK(32),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(1),WORK+2      MEDIA                                         
         MVC   KEY+1(6),WORK+10      PUB                                        
         MVC   KEY+7(2),WORK         AGY                                        
         MVI   KEY+9,X'81'                                                      
         GOTO1 HIGHPUB                                                          
         CLC   KEY(10),KEYSAVE                                                  
         BE    PROC8                                                            
         GOTO1 HEXOUT,DMCB,KEYSAVE,P,33,=C'N'                                   
         BAS   RE,RPRT                                                          
*                                                                               
PROC8    MVC   KEY,WORK                                                         
         GOTO1 HIGH                                                             
         B     PROC3                                                            
*                                                                               
         LA    R7,PBUYREC                                                       
         ST    R7,AREC                                                          
         GOTO1 GETPRT                                                           
         CLC   KEY(15),0(R7)                                                    
         BE    PROC3                                                            
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY,P,33,=C'N'                                       
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,0(R7),P,65,=C'N'                                     
         BAS   RE,RPRT                                                          
         B     PROC3                                                            
*                                                                               
RUNL     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R4,COUNTS                                                        
RUNL5    CLI   0(R4),X'FF'                                                      
         BE    EXIT                                                             
         MVC   P(15),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,23(R4)                                                        
         B     RUNL5                                                            
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                             LINK TO REPORT                                    
         SPACE 2                                                                
         DS    0F                                                               
RPRT     NTR1                                                                   
         SPACE 2                                                                
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
         SPACE 2                                                                
PP02WRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
FRSTSW   DS    XL1                                                              
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
ITOT     DS    F                                                                
SKEY     DS    CL64                                                             
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003RJSCONT   05/01/02'                                      
         END                                                                    
