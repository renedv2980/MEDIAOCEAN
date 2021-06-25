*          DATA SET SPREPFF02  AT LEVEL 004 AS OF 05/03/11                      
*PHASE SPFF02A                                                                  
*INCLUDE PRTREC                                                                 
         TITLE 'SPFF02 - FIXFILMS FOR INVOICES'                                 
SPFF02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFF02,RR=R2                                                   
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPFF02+4096,RC                                                   
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         BRAS  RE,SETUP                                                         
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,CLTFRST                                                     
         BE    CLTF                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
NEQXIT   LTR   RB,RB                                                            
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
* REQFRST                                                                       
*                                                                               
REQF     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
*                                                                               
* CLTFRST                                                                       
*                                                                               
CLTF     DS    0H                                                               
         MVC   HEADHOOK,=A(HDHK)                                                
         MVI   FFTEST,C'N'                                                      
         CLI   RCWRITE,C'Y'                                                     
         BE    *+8                                                              
         MVI   FFTEST,C'Y'                                                      
         CLI   QOPT2,C'N'                                                       
         BE    *+8                                                              
         MVI   FFTEST,C'Y'                                                      
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,(CCALLOV-COMFACSD)(RF)                                        
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'                                           
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   TRPACK,0(R1)            ADDRESS OF TRPACK                        
         XC    COUNT,COUNT                                                      
*                                                                               
         XC    MOS,MOS             OPTIONAL MOS FILTER                          
         CLI   QEST+2,C' '                                                      
         BNH   CLTF00                                                           
         OC    QEST+2(6),=6C'0'                                                 
         MVI   QEST+7,C'1'                                                      
         GOTO1 DATCON,DMCB,QEST+2,(2,MOS)                                       
         XC    MOS,=X'FFFF'                                                     
*                                                                               
*                                                                               
* READ INVOICES                                                                 
*                                                                               
CLTF00   LA    R3,INVKEY                                                        
         USING SNVKEY,R3                                                        
*                                                                               
         XC    INVKEY,INVKEY                                                    
         MVC   INVKEY(2),=X'0E03'                                               
         MVC   SNVKAM,BAGYMD                                                    
         CLC   QCLT,=C'ALL'                                                     
         BE    *+10                                                             
         MVC   SNVKCLT,BCLT                                                     
         MVC   INVKEYSV,INVKEY                                                  
*                                                                               
INVHI    GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',INVKEY,INVKEY,0               
         B     INV05                                                            
*                                                                               
INVSEQ   GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',INVKEY,INVKEY,0               
*                                                                               
INV05    DS    0H                                                               
         LA    R3,INVKEY                                                        
         CLC   SNVKEY(3),INVKEYSV     X'0E03',A/M                               
         BNE   INVX                                                             
*                                                                               
         CLC   QCLT,=C'ALL'                                                     
         BE    *+14                                                             
         CLC   SNVKCLT,BCLT                                                     
         BNE   INVX                                                             
*                                                                               
         OC    MOS,MOS                                                          
         BZ    INV06                                                            
         CLC   SNVKMOS,MOS                                                      
         BE    INV06                                                            
         MVC   SNVKMOS+L'SNVKMOS,=X'FFFF'                                       
         MVC   INVKEYSV,INVKEY                                                  
         B     INVHI                                                            
*                                                                               
INV06    DS    0H                                                               
         MVI   PRTFLAG,C'I'                                                     
*                                                                               
         MVC   INVKEYSV,INVKEY                                                  
         MVC   SNVDA,SNVDDA                                                     
         MVC   SNVCLT,SNVKCLT                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',SNVDA,ADBUY,DMWORK            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   FIXED,C'N'                                                       
*                                                                               
         L     R3,ADBUY                                                         
         MVI   ELCODE,SNVCMELQ     X'30' COMML ELEMS                            
         LR    R6,R3                                                            
         USING SNVCMELD,R6                                                      
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
INVNXT   BAS   RE,NEXTEL                                                        
         BNE   INV50                                                            
*                                                                               
         OC    SNVCMSEQ,SNVCMSEQ   SEQ NO. FILLED IN?                           
         BZ    *+12                NO - FIX IT                                  
         CLI   QOPT1,C'Y'          YES, SEE IF WE'RE FIXIN'EM ALL               
         BNE   INVNXT              FIXING ZEROES ONLY - SKIP THIS ONE           
*                                                                               
         MVC   QFILM,SNVCMCD                                                    
         XC    RDSEQ,RDSEQ         SEQ # FROM COMML RECORD                      
         MVI   HDFLAG,C'N'                                                      
*                                                                               
         XC    KEYSAVE,KEYSAVE                                                  
*                                                                               
INV06A   XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING CMLRECD,R2                                                       
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM,BAGYMD                                                    
         MVC   CMLKCLT,SNVCLT                                                   
         MVC   CMLKCML,QFILM                                                    
*                                                                               
         CLI   QFILM+8,C' '        AD-ID?                                       
         BNH   INV07                                                            
*                                                                               
         CLC   KEYSAVE(2),=X'0AC1'                                              
         BE    *+14                                                             
         MVC   CMLPIDAD,=X'0AC1'                                                
         B     *+10                                                             
         MVC   CMLPIDAD,=X'0AC2'                                                
*                                                                               
         GOTO1 TRPACK,DMCB,(C'P',QFILM),CMLPADID                                
*                                                                               
INV07    DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    INV08                                                            
*                                                                               
         CLI   QFILM+8,C' '        AD-ID?                                       
         BNH   CML20                                                            
         CLC   KEYSAVE(2),=X'0AC1'                                              
         BE    INV06A                                                           
         B     CML20                                                            
*                                                                               
INV08    DS    0H                                                               
         CLC   =X'0A21',KEY                                                     
         BNE   *+12                                                             
         TM    KEY+CMLKSTAT-CMLKEY,CMLKSTA_PCKD                                 
         BO    CML20                                                            
*                                                                               
         L     R1,AIO                                                           
         ST    R1,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         L     R2,AREC                                                          
*                                                                               
         CLC   KEY(2),=X'0AC2'     HAVE HD COMMERCIAL?                          
         BNE   *+8                                                              
         MVI   HDFLAG,C'Y'                                                      
*                                                                               
         AHI   R2,24                                                            
*                                                                               
CML10    CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                DIE IF THERE ISN'T ONE                       
*                                                                               
         CLI   0(R2),X'10'         X'10' ELEM                                   
         BNE   CML15                                                            
         TM    CMLSTAT-CMLDTAEL(R2),X'80'   CML DELETED?                        
         BO    CML20                                                            
         MVC   RDSEQ,CMLSEQ-CMLDTAEL+1(R2)                                      
         B     CML20                                                            
*                                                                               
CML15    ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     CML10                                                            
*                                                                               
CML20    DS    0H                                                               
         CLC   SNVCMSEQ,RDSEQ      SEQ NO. SAME ON NINV, COMML RECORD?          
         BNE   CML30               NO - FIX IT                                  
*                                                                               
* NOW CHECK IF THE HD FLAG IS SET CORRECTLY                                     
*                                                                               
         CLI   HDFLAG,C'Y'         HAVE HD COMMERCIAL?                          
         BNE   *+16                NO - MAKE SURE FLAG IS *NOT* SET             
         TM    SNVCMFLG,SNVCMHDQ   YES, IS THE HD FLAG SET?                     
         BZ    CML30               NO IT ISN'T - GO FIX IT                      
         B     INVNXT              FLAG IS SET, OUR COMMERCIAL IS OK            
*                                                                               
         TM    SNVCMFLG,SNVCMHDQ   THIS IS HD, IS THE HD FLAG SET?              
         BZ    INVNXT              IT ISN'T - WE'RE OK                          
*                                                                               
CML30    DS    0H                                                               
         MVC   OLDSEQ,SNVCMSEQ     SAVE ORIGINAL SEQ NO.                        
         MVC   OLDFLG,SNVCMFLG     SAVE ORIGINAL COMML FLAG                     
         MVC   SNVCMSEQ,RDSEQ      SET COMMERCIAL SEQ NO.                       
*                                                                               
         NI    SNVCMFLG,X'FF'-SNVCMHDQ                                          
         CLI   HDFLAG,C'Y'                                                      
         BNE   *+8                                                              
         OI    SNVCMFLG,SNVCMHDQ                                                
*                                                                               
         MVI   FIXED,C'Y'                                                       
*                                                                               
         BAS   RE,PRTINV           PRINT INVOICE DETAILS                        
         MVI   PRTFLAG,C'F'                                                     
*                                                                               
         B     INVNXT              NEXT INVOICE COMML                           
*                                                                               
* ALL X'30' ELEMENTS PROCESSED                                                  
*                                                                               
INV50    CLI   FIXED,C'Y'          HAVE ANY SEQ NUMBERS BEEN UPDATED?           
         BNE   INV55               NO - READ NEXT INVOICE                       
*                                                                               
         L     R6,AIO2                                                          
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',SNVDA,(R6),DMWORK             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   QOPT5,C'Y'                                                       
         BNE   INV54                                                            
*                                                                               
         L     R6,AIO2                                                          
         GOTO1 =V(PRTREC),DMCB,(C'E',(R6)),(42,32),PRINT,HEXOUT                 
         GOTO1 REPORT                                                           
         GOTO1 =V(PRTREC),DMCB,(C'E',ADBUY),(42,32),PRINT,HEXOUT                
         GOTO1 REPORT                                                           
*                                                                               
INV54    CLI   FFTEST,C'Y'         AND WRITE OUT RECORD                         
         BE    INV55                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'XSPFIL',SNVDA,ADBUY,DMWORK            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
INV55    B     INVSEQ                                                           
*                                                                               
INVX     DS    0H                                                               
         GOTO1 AENDREQ                                                          
*                                                                               
*        PRINT OUT INVOICE DETAILS                                              
*                                                                               
PRTINV   NTR1                                                                   
         CLI   PRTFLAG,C'I'                                                     
         BNE   PRTI10                                                           
*                                                                               
         GOTO1 CLUNPK,DMCB,SNVKCLT,PCLT                                         
*                                                                               
         MVC   WORK(2),=X'0001'                                                 
         MVC   WORK+2(3),SNVKSTA                                                
         GOTO1 MSUNPK,DMCB,WORK,WORK+10,PSTA                                    
*                                                                               
         MVC   WORK(2),SNVKMOS                                                  
         XC    WORK(2),=X'FFFF'                                                 
         GOTO1 DATCON,DMCB,(2,WORK),(6,TEMP)                                    
         MVC   PMOS(6),TEMP                                                     
*                                                                               
         MVC   PINV,SNVKINV                                                     
*                                                                               
PRTI10   DS    0H                                                               
         MVC   PFILM,QFILM                                                      
*                                                                               
         EDIT  OLDSEQ,(6,PSEQOLD),ALIGN=RIGHT,ZERO=NOBLANK                      
*                                                                               
         EDIT  RDSEQ,(6,PSEQNEW),ALIGN=RIGHT,ZERO=NOBLANK                       
*                                                                               
         CLI   QOPT1,C'Y'                                                       
         BNE   PRTI20                                                           
*                                                                               
         TM    OLDFLG,SNVCMHDQ     WAS HD FLAG SET ORIGINALLY?                  
         BO    *+20                YES                                          
         CLI   HDFLAG,C'Y'         NO - IS IT SET NOW?                          
         BNE   PRTI20              NO                                           
         MVI   PHD,C'+'            YES - PRINT A '+'                            
         B     PRTI20                                                           
*                                                                               
         CLI   HDFLAG,C'Y'         FLAG ON ORIGINALLY, IS IT SET NOW?           
         BE    *+8                 YES                                          
         MVI   PHD,C'-'            NO - PRINT A '-'                             
*                                                                               
PRTI20   DS    0H                                                               
         OC    P,SPACES                                                         
*                                                                               
         GOTO1 REPORT                                                           
         XIT1                                                                   
*                                                                               
*                                                                               
HDHK     NTR1                                                                   
         MVC   H1(9),=CL9'REQUESTOR'                                            
         MVC   H1+10(10),QUESTOR                                                
         MVC   H1+50(27),=CL27'NEW INVOICE FIXFILMS REPORT'                     
         MVC   H1+98(22),AGYNM                                                  
*                                                                               
         MVC   H2(10),MEDNM                                                     
         MVC   H2+50(27),=27C'-'                                                
         MVC   H2+98(22),AGYADR                                                 
*                                                                               
         MVC   H3+98(4),=C'PAGE'                                                
         EDIT  (B2,PAGE),(4,H3+103),ALIGN=LEFT                                  
*                                                                               
         LA    R2,H4                                                            
         USING PLINE,R2                                                         
         MVC   PCLT,=C'CLT'                                                     
         MVC   PSTA,=CL5'STA'                                                   
         MVC   PMOS(6),=CL6'MOS'                                                
         MVC   PINV,=CL10'INVOICE'                                              
         MVC   PFILM,=CL12'FILMCODE'                                            
         MVC   PSEQOLD,=C'OLD SQ'                                               
         MVC   PSEQNEW,=C'NEW SQ'                                               
*                                                                               
         CLI   QOPT1,C'Y'          YES, SEE IF WE'RE FIXIN'EM ALL               
         BNE   *+10                                                             
         MVC   PHD(2),=C'HD'                                                    
*                                                                               
         LA    R2,H5                                                            
         MVC   PCLT,=3C'-'                                                      
         MVC   PSTA,=5C'-'                                                      
         MVC   PMOS,=6C'-'                                                      
         MVC   PINV,=10C'-'                                                     
         MVC   PFILM,=12C'-'                                                    
         MVC   PSEQOLD,=6C'-'                                                   
         MVC   PSEQNEW,=6C'-'                                                   
*                                                                               
         CLI   QOPT1,C'Y'          YES, SEE IF WE'RE FIXIN'EM ALL               
         BNE   *+10                                                             
         MVC   PHD(2),=C'--'                                                    
*                                                                               
         XIT1                                                                   
         DROP  R2                                                               
*                                                                               
*                                                                               
         DROP  R3                                                               
REQL     MVC   P(17),=C'NUMBER OF RECORDS'                                      
         EDIT  COUNT,(10,P+20),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
         GETEL R6,42,ELCODE                                                     
         EJECT                                                                  
*                                                                               
*                                                                               
SETUP    NTR1  BASE=*,LABEL=*                                                   
         L     RF,=A(IO)                                                        
         ST    RF,AIO                                                           
         L     RF,=A(IO2)                                                       
         ST    RF,AIO2                                                          
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
AIO      DS    A                                                                
AIO2     DS    A                                                                
FFTEST   DS    C                                                                
ELCODE   DS    X                                                                
FIXED    DS    C                                                                
HDFLAG   DS    C                                                                
PRTFLAG  DS    C                   I=WHOLE INVOICE,F=FILM                       
RDSEQ    DS    XL2                                                              
MOS      DS    XL2                                                              
COUNT    DS    F                                                                
TRPACK   DS    A                                                                
TEMP     DS    CL80                                                             
QFILM    DS    CL12                                                             
SNVDA    DS    XL4                                                              
SNVCLT   DS    XL2                                                              
INVKEY   DS    CL50                                                             
INVKEYSV DS    CL50                                                             
OLDSEQ   DS    XL2                                                              
OLDFLG   DS    X                                                                
*                                                                               
IO       DS    6000X                                                            
IO2      DS    6000X                                                            
*                                                                               
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENSNV                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPTRCMML                                                       
       ++INCLUDE SPREPWORKD                                                     
*                                                                               
* DSECT FOR PRINT LINE                                                          
         ORG   P                                                                
*                                                                               
PLINE    DS    0X                                                               
         DS    CL1                                                              
PCLT     DS    CL3                                                              
         DS    CL3                                                              
PSTA     DS    CL5                                                              
         DS    CL3                                                              
PMOS     DS    CL8                                                              
         DS    CL3                                                              
PINV     DS    CL10                                                             
         DS    CL3                                                              
PFILM    DS    CL12                                                             
         DS    CL3                                                              
PSEQOLD  DS    CL6                                                              
         DS    CL3                                                              
PSEQNEW  DS    CL6                                                              
         DS    CL3                                                              
PHD      DS    C                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPREPFF02 05/03/11'                                      
         END                                                                    
