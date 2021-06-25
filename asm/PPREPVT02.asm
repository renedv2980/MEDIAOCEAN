*          DATA SET PPREPVT02  AT LEVEL 002 AS OF 05/01/02                      
*PHASE PPVT02A,+0,NOAUTO                                                        
         TITLE 'PPREPVT02 - VALUTECH INTERFACE'                                 
***********************************************************************         
*   QOPT1 -  D=DUMP OUTPUT RECORDS                                              
*                                                                               
*   QOPT7 - Y= TRACE BUFFALO PUTS,READS                                         
*                                                                               
***********************************************************************         
PPVT02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPVT02                                                         
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING BILWRKD,RC                                                       
         L     R8,PPFILEC                                                       
         LA    R9,4095(R8)                                                      
         LA    R9,1(R9)                                                         
         USING PPFILED,R8,R9                                                    
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,PROCBUY                                                     
         BE    PROCBY                                                           
*                                                                               
         CLI   MODE,LBUYCLI                                                     
         BE    CLTL                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         SPACE 2                                                                
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*        RUNFRST                                                                
         SPACE 2                                                                
RUNF     DS    0H                                                               
*                                  RELOCATE ADDRESSES                           
         RELOC RELO                                                             
         LA    R0,(ACONSX-ACONS)/4      NO. OF ADDRS                            
         LA    R2,ACONS                                                         
         LA    R3,RCONS                                                         
RUNF2    DS    0H                                                               
         L     RF,0(R2)                                                         
         A     RF,RELO                                                          
         ST    RF,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,RUNF2                                                         
*                                                                               
         MVI   FFS,X'FF'                                                        
         MVC   FFS+1(L'FFS-1),FFS                                               
         MVI   DASHES,C'-'                                                      
         MVC   DASHES+1(L'DASHES-1),DASHES                                      
*                                                                               
*                                  SET BUFFALO PARAMS                           
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFFC                                      
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
*        REQFRST                                                                
         SPACE 2                                                                
REQF     DS    0H                                                               
         MVC   SVQOPT1,QOPT1                                                    
         CLC   =C'ALL',QEST                                                     
         BNE   *+10                                                             
         MVC   QEST,SPACES                                                      
         MVI   FCRDACTV,C'N'                                                    
         CLI   QPRODUCT,C' '                                                    
         BE    INITA                                                            
         CLC   QPRODUCT,=C'ALL'                                                 
         BNE   *+8                                                              
*                                                                               
INITA    MVI   FCRDACTV,C'Y'                                                    
         OI    DMINBTS,X'08'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
*                                                                               
         MVC   MYSTART,QSTART                                                   
         MVC   MYEND,QEND                                                       
         GOTO1 DATCON,DMCB,QSTART,(3,MYSTRTB)                                   
         GOTO1 DATCON,DMCB,QEND,(3,MYENDB)                                      
*                                                                               
         MVC   QSTART,SPACES                                                    
         MVC   QEND,SPACES                                                      
         MVC   BQSTART,=X'000000'                                               
         MVC   BQEND,=X'FFFFFF'                                                 
*                                                                               
         ZAP   QTPUBS,=P'0'                                                     
         ZAP   QTGRS,=P'0'                                                      
         ZAP   QTNET,=P'0'                                                      
         ZAP   QTCD,=P'0'                                                       
*                                                                               
         CLI   FIRST,0             FIRST TIME TEST                              
         BNE   REQF20                                                           
         MVI   FIRST,1                                                          
*                                                                               
         ZAP   GTPUBS,=P'0'                                                     
         ZAP   GTGRS,=P'0'                                                      
         ZAP   GTNET,=P'0'                                                      
         ZAP   GTCD,=P'0'                                                       
*                                                                               
         LA    R5,OUTFILE                                                       
         OPEN  ((R5),OUTPUT)                                                    
*                                                                               
REQF20   DS    0H                                                               
         B     EXIT                                                             
         SPACE 3                                                                
PROCBY   GOTO1 APRBUY                                                           
         B     EXIT                                                             
         SPACE 3                                                                
*        CLTFRST                                                                
CLTF     DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        PRDFRST                                                                
PRDF     DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        ESTFRST                                                                
ESTF     DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        PRDLAST                                                                
PRDL     DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        CLT LAST                                                               
         SPACE 2                                                                
CLTL     DS    0H                                                               
         B     EXIT                                                             
         SPACE 3                                                                
*        REQ LAST                                                               
REQL     DS    0H                                                               
         GOTO1 AREPRT                                                           
         GOTO1 APRNT                                                            
         LA    R2,P                                                             
         USING LINED,R2                                                         
         MVC   P(20),=C'** REQUEST TOTALS **'                                   
         EDIT  (P8,QTPUBS),(14,LPUBN),COMMAS=YES                                
         MVC   LPUBN+16(7),=C'VENDORS'                                          
         EDIT  (P8,QTNET),(14,LNET),2,COMMAS=YES,MINUS=YES                      
         EDIT  (P8,QTGRS),(14,LGRS),2,COMMAS=YES,MINUS=YES                      
         EDIT  (P8,QTCD),(14,LCD),2,COMMAS=YES,MINUS=YES                        
         MVI   SPACING,2                                                        
         GOTO1 APRNT                                                            
         B     EXIT                                                             
         SPACE 3                                                                
*        RUN LAST                                                               
RUNL     DS    0H                                                               
         GOTO1 APRNT                                                            
         LA    R2,P                                                             
         USING LINED,R2                                                         
         MVC   P(16),=C'** RUN TOTALS **'                                       
         EDIT  (P8,GTPUBS),(14,LPUBN),COMMAS=YES                                
         MVC   LPUBN+16(7),=C'VENDORS'                                          
         EDIT  (P8,GTNET),(14,LNET),2,COMMAS=YES,MINUS=YES                      
         EDIT  (P8,GTGRS),(14,LGRS),2,COMMAS=YES,MINUS=YES                      
         EDIT  (P8,GTCD),(14,LCD),2,COMMAS=YES,MINUS=YES                        
         MVI   SPACING,2                                                        
         GOTO1 APRNT                                                            
*                                                                               
*                                                                               
         CLI   ERROR,0                                                          
         BE    RUNL4                                                            
         MVC   P(45),=C'***** ERRORS - NO OUTPUT FILE GENERATED *****'          
         GOTO1 APRNT                                                            
         B     RUNLX                                                            
*                                                                               
RUNL4    DS    0H                                                               
*                                                                               
RUNL8    DS    0H                                                               
         CLOSE (OUTFILE)                                                        
         CLI   SVQOPT1,C'D'       SEE IF DUMPING OUTPUT                         
         BNE   RUNL10                                                           
         OPEN  (OUTFILR,INPUT)                                                  
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
RUNL9    DS    0H                                                               
         LA    R1,OUTFILR                                                       
         LA    R0,OUTREC                                                        
         GET   (R1),(R0)                                                        
         MVC   P(100),OUTREC                                                    
         GOTO1 APRNT                                                            
         MVC   P(4),=C'HEX='                                                    
         GOTO1 HEXOUT,DMCB,OUTREC,P+10,50,=C'N'                                 
         GOTO1 HEXOUT,DMCB,OUTREC+50,PSECOND+10,50,=C'N'                        
         MVI   SPACING,2                                                        
         GOTO1 APRNT                                                            
         B     RUNL9                                                            
RUNL10   DS    0H                                                               
RUNLX    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
OUTFEOD  DS    0H                                                               
         CLOSE (OUTFILR)                                                        
         B     RUNLX                                                            
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
ACONS    DS    0F                                                               
         DC    A(PRNT)                                                          
         DC    A(PRBUY)                                                         
         DC    A(REPRT)                                                         
         DC    A(BUFFALOC)                                                      
ACONSX   EQU   *                                                                
         SPACE 2                                                                
OUTFILE  DCB   DDNAME=PPVTXOUT,DSORG=PS,RECFM=FB,LRECL=082,            X        
               BLKSIZE=0820,MACRF=PM                                            
OUTFILR  DCB   DDNAME=PPVTXOUT,DSORG=PS,RECFM=FB,LRECL=082,            X        
               BLKSIZE=0820,MACRF=GM,EODAD=OUTFEOD                              
*                                                                               
         EJECT                                                                  
PRBUY    CSECT                                                                  
         NMOD1 0,PRBUY                                                          
         LA    RC,SPACEND                                                       
RNB10    DS    0H                                                               
         XC    BUYGRS,BUYGRS                                                    
         XC    BUYAC,BUYAC                                                      
         XC    BUYCD,BUYCD                                                      
         MVI   ELCODE,X'25'                                                     
         LA    R2,PBDELEM                                                       
         USING PPAYELEM,R2                                                      
*                                                                               
RNB11    DS    0H                                                               
         CLC   ELCODE,0(R2)                                                     
         BE    RNB13                                                            
*                                                                               
RNB12    DS    0H                                                               
         BAS   RE,RNBNXTEL                                                      
         BNE   RNB13T                                                           
*                                                                               
RNB13    DS    0H                                                               
*                                  TEST IN REQ PERIOD                           
         CLC   PPDDATE,MYSTRTB                                                  
         BL    RNB12                                                            
         CLC   PPDDATE,MYENDB                                                   
         BH    RNB12                                                            
         L     R0,BUYGRS                                                        
         A     R0,PPGROSS                                                       
         ST    R0,BUYGRS                                                        
         L     R0,BUYAC                                                         
         A     R0,PPAGYCOM                                                      
         ST    R0,BUYAC                                                         
         L     R0,BUYCD                                                         
         A     R0,PPCSHDSC                                                      
         ST    R0,BUYCD                                                         
         B     RNB12                                                            
         DROP  R2                                                               
*                                                                               
RNB13T   DS    0H                                                               
         OC    BUYGRS(12),BUYGRS                                                
         BZ    RNB30                                                            
*                                                                               
         XC    X,X                                                              
         LA    R4,X                                                             
         USING INVD,R4                                                          
         ZAP   INVGRS,=P'0'                                                     
         ZAP   INVNET,=P'0'                                                     
         ZAP   INVCD,=P'0'                                                      
*                                                                               
*                                                                               
RNB15    DS    0H                                                               
         MVC   INVMED,PBUYKMED                                                  
         MVC   INVPUB,PBUYKPUB                                                  
         MVC   INVPNAM,PUBNAME                                                  
         MVC   INVMKT,PUBZNAME                                                  
         CLI   PBUYKMED,C'O'             SEE IF OUTDOOR                         
         BE    RNB18                                                            
         XC    INVMKT,INVMKT                                                    
         CLI   PBUYKMED,C'N'             SEE IF NEWSPAPERS                      
         BNE   RNB18                                                            
         MVC   INVMKT,PUBSTATE                                                  
         MVI   INVMKT+2,C','                                                    
         MVC   INVMKT+4(16),PUBCITY                                             
*                                                                               
RNB18    DS    0H                                                               
         ZAP   INVCDP,=P'0'                                                     
         ZAP   INVCDDA,=P'0'                                                    
         LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'20'                                                     
         BAS   RE,RNBNXTEL                                                      
         BNE   RNB20                                                            
         USING PUBGENEL,R2                                                      
         ZAP   INVCDP,PUBCD                                                     
         ZAP   INVCDDA,PUBCDDAS                                                 
         DROP  R2                                                               
*                                                                               
RNB20    DS    0H                                                               
         L     R0,BUYGRS                                                        
         CVD   R0,DUB                                                           
         ZAP   INVGRS,DUB                                                       
         L     R0,BUYGRS                                                        
         CVD   R0,DUB                                                           
         ZAP   INVNET,DUB                                                       
         L     R0,BUYAC                                                         
         CVD   R0,DUB                                                           
         SP    INVNET,DUB                                                       
         L     R0,BUYCD                                                         
         CVD   R0,DUB                                                           
         ZAP   INVCD,DUB                                                        
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFFC,X                                    
*                                                                               
         CLI   QOPT7,C'Y'          SEE IF TRACING                               
         BNE   RNB30                                                            
         MVC   P(11),=C'**BUFF IN**'                                            
         GOTO1 APRNT                                                            
         GOTO1 HEXOUT,DMCB,X,P+5,54,=C'N'                                       
         GOTO1 HEXOUT,DMCB,X+54,PSECOND+5,54,=C'N'                              
         GOTO1 APRNT                                                            
         B     RNB30                                                            
*                                                                               
RNB30    DS    0H                                                               
*                                                                               
RNB40    DS    0H                                                               
RNBX     DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
RNBNXTEL DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    RNBNXTL2                                                         
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     RNBNXTEL                                                         
RNBNXTL2 DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
         LTORG                                                                  
         TITLE 'REPRT - CREATE TAPE RECS AND PRINT REPORT'                      
REPRT    CSECT                                                                  
         NMOD1 0,REPRT                                                          
         LA    RC,SPACEND                                                       
         SPACE 2                                                                
         XC    SVHDR,SVHDR                                                      
         MVI   FORCEHED,C'Y'                                                    
         LA    R2,P                                                             
         USING LINED,R2                                                         
         XC    BUFFREC,BUFFREC                                                  
         LA    R4,BUFFREC                                                       
         USING INVD,R4                                                          
REP2     DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFFC,BUFFREC,0                           
         B     REP4B                                                            
REP4     DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFFC,BUFFREC,0                            
REP4B    DS    0H                                                               
         TM    DMCB+8,X'80'                                                     
         BNZ   REP50                                                            
*                                                                               
         CLI   QOPT7,C'Y'          SEE IF TRACING                               
         BNE   REP6                                                             
         MVC   P(12),=C'**BUFF OUT**'                                           
         GOTO1 APRNT                                                            
         GOTO1 HEXOUT,DMCB,BUFFREC,P+5,54,=C'N'                                 
         GOTO1 HEXOUT,DMCB,BUFFREC+54,PSECOND+5,54,=C'N'                        
         GOTO1 APRNT                                                            
*                                                                               
REP6     DS    0H                                                               
*                                                                               
         BAS   RE,DETOUT             OUTPUT RECORD                              
*                                                                               
         GOTO1 PUBEDIT,DMCB,INVPUB,LPUB                                         
         MVC   LPUBN,INVPNAM                                                    
         MVC   LMKT,INVMKT                                                      
         EDIT  (P2,INVCDP),(5,LCDP),1,COMMAS=YES,MINUS=YES                      
         EDIT  (P2,INVCDDA),(5,LCDD),0,COMMAS=YES,MINUS=YES                     
*                                                                               
REP22    DS    0H                                                               
         EDIT  (P8,INVGRS),(14,LGRS),2,COMMAS=YES,MINUS=YES                     
         EDIT  (P8,INVNET),(14,LNET),2,COMMAS=YES,MINUS=YES                     
         EDIT  (P8,INVCD),(14,LCD),2,COMMAS=YES,MINUS=YES                       
*                                                                               
         AP    QTGRS,INVGRS                                                     
         AP    GTGRS,INVGRS                                                     
         AP    QTNET,INVNET                                                     
         AP    GTNET,INVNET                                                     
         AP    QTCD,INVCD                                                       
         AP    GTCD,INVCD                                                       
         AP    QTPUBS,=P'1'                                                     
         AP    GTPUBS,=P'1'                                                     
*                                                                               
         GOTO1 APRNT                                                            
*                                                                               
REP30    DS    0H                                                               
         B     REP4                                                             
*                                                                               
REP50    DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'RESET',ABUFFC                                    
*                                                                               
REPX     DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
         EJECT                                                                  
*        DETAIL OUTPUT                                                          
         SPACE 2                                                                
DETOUT   NTR1                                                                   
         LA    R3,OUTREC                                                        
         USING VTOUTD,R3                                                        
         MVI   VTDATA,C' '                                                      
         MVC   VTDATA+1(L'VTDATA-1),VTDATA                                      
*                                                                               
         MVC   VTPUB(2),=C'SP'                                                  
         MVC   VTPUB+2(1),INVMED                                                
         GOTO1 PUBEDIT,DMCB,INVPUB,(C'Q',VTPUB+3)                               
         MVC   VTPUBN,INVPNAM                                                   
         MVC   VTMKT,INVMKT                                                     
         ZAP   DUB,INVNET                                                       
         SP    DUB,INVCD                                                        
         UNPK  VTYTD,DUB             NET LESS CD                                
*                                                                               
DETO7    DS    0H                                                               
         L     R1,=A(OUTFILE)                                                   
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
*                                                                               
DETO8    DS    0H                                                               
         DROP  R3                                                               
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         TITLE 'PRNT - PRINT CONTROL MODULE'                                    
PRNT     CSECT                                                                  
         NMOD1 0,PRNT                                                           
         LA    RC,SPACEND                                                       
         MVC   HEAD3+50(4),=C'FROM'                                             
         GOTO1 DATCON,DMCB,(0,MYSTART),(5,HEAD3+55)                             
         MVC   HEAD3+64(2),=C'TO'                                               
         GOTO1 (RF),(R1),(0,MYEND),(5,HEAD3+67)                                 
         GOTO1 REPORT                                                           
*                                                                               
PRNTX    DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*       BILLING WORK AREA DSECT                                                 
BILWRKD  DSECT                                                                  
BILWRK   DS    0C                                                               
RCONS    DS    0F                                                               
APRNT    DS    A                                                                
APRBUY   DS    A                                                                
AREPRT   DS    A                                                                
ABUFFC   DS    A                                                                
*                                                                               
RELO     DS    A                                                                
*                                                                               
PEPPARS  DS    F                                                                
APEPTAB  DS    F                                                                
PEPTABN  DS    F                                                                
         DS    3F                                                               
*                                                                               
BRKTAB   DS    CL240               TABLE OF SORT/BREAK CONTROLS                 
SAVPRDC  DS    CL3                                                              
SAVPRD   DS    X                                                                
SAVPGR   DS    XL2                                                              
SAVPGRU  DS    CL4                                                              
SAVMKT   DS    XL2                                                              
SAVMGR   DS    XL2                                                              
SAVMGRU  DS    CL4                                                              
         DS    0F                                                               
X        DS    XL200                                                            
W        DS    CL132                                                            
SAVMODE  DS    X                                                                
MANRVNO  DS    H                                                                
MANMOS   DS    H                                                                
         DS    0F                                                               
INTC     DS    F                                                                
INTSTAT  DS    X                                                                
BUFFREC  DS    XL75                                                             
OLDKEY   DS    XL75                                                             
KPRD     DS    X                                                                
KMKT     DS    XL2                                                              
KSTA     DS    XL3                                                              
HOLDPRD  DS    XL1                                                              
HOLDPRD2 DS    XL1                                                              
SAVR1    DS    F                                                                
FFS      DS    XL6'FF'                                                          
DASHES   DS    CL35'-'                                                          
ELCODE   DS    X                                                                
RECSW    DS    X                                                                
ERR      DS    X                                                                
TOTSTAR  DS    CL2                                                              
BILDAT   DS    H                                                                
INVNO    DS    H                                                                
PINVNO   DS    CL10                                                             
SINVNO   DS    H                                                                
SPINVNO  DS    CL10                                                             
MYSTART  DS    CL6               ORIGINAL START AND END                         
MYEND    DS    CL6                                                              
MYSTRTB  DS    CL3               ORIGINAL START AND END - BINARY                
MYENDB   DS    CL3                                                              
*                                                                               
PSTART   DS    CL8                                                              
PEND     DS    CL8                                                              
SVHDR    DS    XL(INVRL)                                                        
FIRST    DS    X                                                                
ERROR    DS    X                                                                
SVQOPT1  DS    C                                                                
*                                                                               
BUYGRS   DS    F              BUY TOTALS FROM PAYELEMS                          
BUYAC    DS    F                                                                
BUYCD    DS    F                                                                
*                                                                               
GTPUBS   DS    PL8                                                              
GTGRS    DS    PL8                                                              
GTNET    DS    PL8                                                              
GTCD     DS    PL8                                                              
*                                                                               
QTPUBS   DS    PL8                                                              
QTGRS    DS    PL8                                                              
QTNET    DS    PL8                                                              
QTCD     DS    PL8                                                              
*                                                                               
OUTREC   DS    XL(L'VTDATA)                                                     
*                                                                               
LINED    DSECT                                                                  
         DS    CL1                                                              
LPUB     DS    CL17                                                             
         DS    CL1                                                              
LPUBN    DS    CL20                                                             
LMKT     DS    CL20                                                             
         DS    CL1                                                              
LCDP     DS    CL5                                                              
         DS    CL1                                                              
LCDD     DS    CL5                                                              
         DS    CL1                                                              
LGRS     DS    CL14                                                             
         DS    CL1                                                              
LNET     DS    CL14                                                             
         DS    CL1                                                              
LCD      DS    CL14                                                             
         DS    CL1                                                              
         DS    0C                                                               
         SPACE 3                                                                
*                                  DSECT FOR TABLE ENTRY                        
INVD     DSECT                                                                  
INVMED   DS    CL1               MEDIA                                          
INVPUB   DS    XL6                                                              
*                                                                               
         ORG                                                                    
INVKL    EQU   *-INVD                                                           
*                                COMMENT DATA                                   
INVPNAM  DS    CL20              PUBNAME (DETAILS)                              
INVMKT   DS    CL20              MARKET CITY/STATE FOR NEWSPAPERS               
INVCDP   DS    PL2               CD PCT                                         
INVCDDA  DS    PL2               CD DAYS                                        
*                                                                               
INVGRS   DS    PL8                                                              
INVNET   DS    PL8                                                              
INVCD    DS    PL8                                                              
INVRL    EQU   *-INVD                                                           
         SPACE 2                                                                
         EJECT                                                                  
*                                  BUFFALO CSECT                                
         BUFF  LINES=7000,ROWS=1,COLUMNS=3,FLAVOR=PACKED,COMMENT=44,   X        
               KEYLIST=(7,A)                                                    
*                                                                               
       ++INCLUDE DDBUFFALOD                                                     
VTOUTD   DSECT                                                                  
*                                                                               
VTDATA   DS    0CL82                                                            
VTPUB    DS    CL16                                                             
VTPUBN   DS    CL20                                                             
VTMKT    DS    CL12               TRUNCATED MARKET                              
*                                                                               
VTYTD    DS    CL11               PAID Y-T-D (NET)                              
*                                                                               
VTCD     DS    CL3                C.D. PCT                                      
VTCDTYP  DS    CL1                C.D.TYPE - SET TO SPACE                       
VTCDDAYS DS    CL4                C.D. DAYS                                     
*                                                                               
VTNETTYP DS    CL1                NET DAY TYPE                                  
VTNDAYS  DS    CL4                NET DAYS                                      
VTTAXCD  DS    CL10               TAX CODE - UNAVAILABLE                        
*                                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPMODEQU                                                       
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002PPREPVT02 05/01/02'                                      
         END                                                                    
