*          DATA SET PPREP4402  AT LEVEL 013 AS OF 05/01/02                      
*PHASE PP4402A,+0                                                               
*INCLUDE RATELOOK                                                               
         TITLE 'PP4402 - PRINTPAK OFF-LINE LIST BUYING'                         
PP4402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP4402                                                         
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,4095(RC)                                                      
         LA    R9,1(R9)                                                         
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP44WKD,R8                                                       
*                                                                               
         CLI   MODE,PROCPUB                                                     
         BE    PRPUB                                                            
         CLI   MODE,FPUBDST                                                     
         BE    DSTF                                                             
         CLI   MODE,FPUBREG                                                     
         BE    REGF                                                             
         CLI   MODE,LPUBDST                                                     
         BE    DSTL                                                             
         CLI   MODE,LPUBREG                                                     
         BE    REGL                                                             
         CLI   MODE,FPUBREQ                                                     
         BE    REQF                                                             
         CLI   MODE,LPUBREQ                                                     
         BE    REQL                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
*        RUN FIRST                                                              
         SPACE 2                                                                
RUNF     DS    0H                                                               
         LA    R1,ACONS                                                         
         LA    R2,RCONS                                                         
         LA    R0,(ACONSX-ACONS)/4                                              
RUNF2    DS    0H                                                               
         L     RF,0(R1)                                                         
         AR    RF,RB                                                            
         ST    RF,0(R2)                                                         
         LA    R1,4(R1)                                                         
         LA    R2,4(R2)                                                         
         BCT   R0,RUNF2                                                         
         MVI   DASHES,C'-'                                                      
         MVC   DASHES+1(L'DASHES-1),DASHES                                      
         MVC   WORK+00(2),RCDATE+6                                              
         MVC   WORK+02(2),RCDATE+3                                              
         MVC   WORK+04(2),RCDATE+0                                              
         GOTO1 DATCON,DMCB,WORK,(3,BTODAY)                                      
         B     EXIT                                                             
         SPACE 2                                                                
*        REQ FIRST                                                              
         SPACE 2                                                                
REQF     DS    0H                                                               
         MVI   FCGTPUB,C'Y'                                                     
         MVI   FRST,0                                                           
         MVI   FORCEHED,C'Y'                                                    
         OI    DMINBTS,X'08'            SET TO PASS DELETES                     
         MVI   DMOUTBTS,X'FD'                                                   
*                                                                               
         LA    R1,ATOTS            CLEAR TOTALS CONTRALS                        
         LA    R0,ATOTSN                                                        
         MVI   0(R1),0                                                          
         LA    R1,4(R1)                                                         
         BCT   R0,*-8                                                           
*                                                                               
         CLI   QDIST,C' '                                                       
         BE    *+8                                                              
         MVI   ADSTTOT,1           SET NEED DIST TOTS                           
*                                                                               
         CLI   QREGION,C' '                                                     
         BE    *+8                                                              
         MVI   AREGTOT,1           REGION                                       
*                                                                               
         MVI   AREQTOT,1           ALWAYS NEED REQ TOTS                         
         MVI   ARUNTOT,1           AND RUN TOTS                                 
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        REGION FIRST                                                           
         SPACE 2                                                                
REGF     DS    0H                                                               
         MVC   P(6),=C'REGION'                                                  
         MVC   P+9(3),PREGKREG                                                  
         MVC   P+13(20),PREGNAME                                                
         BAS   RE,ULINE                                                         
         GOTO1 APRNT                                                            
         B     EXIT                                                             
         SPACE 2                                                                
*        DISTRICT FIRST                                                         
         SPACE 2                                                                
DSTF     DS    0H                                                               
         MVC   P(8),=C'DISTRICT'                                                
         MVC   P+9(3),PDSTKDST                                                  
         MVC   P+13(20),PDSTNAME                                                
         BAS   RE,ULINE                                                         
         GOTO1 APRNT                                                            
         B     EXIT                                                             
         SPACE 3                                                                
*        REGION LAST                                                            
         SPACE 2                                                                
REGL     DS    0H                                                               
         MVC   P(15),=C'*REGION TOTALS*'                                        
         LA    R3,AREGTOT                                                       
         BAS   RE,EDTOTS                                                        
         MVI   SPACING,2                                                        
         GOTO1 APRNT                                                            
         BAS   RE,ROLTOTS                                                       
         B     EXIT                                                             
         SPACE 3                                                                
*        DISTRICT LAST                                                          
         SPACE 2                                                                
DSTL     DS    0H                                                               
         MVC   P(17),=C'*DISTRICT TOTALS*'                                      
         LA    R3,ADSTTOT                                                       
         BAS   RE,EDTOTS                                                        
         MVI   SPACING,2                                                        
         GOTO1 APRNT                                                            
         BAS   RE,ROLTOTS                                                       
         B     EXIT                                                             
         SPACE 3                                                                
*        REQ LAST                                                               
         SPACE 2                                                                
REQL     DS    0H                                                               
         MVC   P(16),=C'*REQUEST TOTALS*'                                       
         LA    R3,AREQTOT                                                       
         BAS   RE,EDTOTS                                                        
         GOTO1 APRNT                                                            
         BAS   RE,ROLTOTS                                                       
         B     EXIT                                                             
         SPACE 3                                                                
*        RUN LAST                                                               
         SPACE 2                                                                
RUNL     DS    0H                                                               
         MVC   P(12),=C'*RUN TOTALS*'                                           
         LA    R3,ARUNTOT                                                       
         BAS   RE,EDTOTS                                                        
         GOTO1 APRNT                                                            
         B     EXIT                                                             
         EJECT                                                                  
*        PROC PUB                                                               
         SPACE 2                                                                
PRPUB    DS    0H                                                               
         CLI   FRST,0                                                           
         BNE   PP20                                                             
         BAS   RE,SETDATS                                                       
         XC    BILLPROF,BILLPROF                                                
*                                  NOTE- CHECK BILLPROF NERE                    
*                                  FIND PROTO BUY                               
         XC    KEY,KEY                                                          
         MVC   KEY(10),PPRDKEY                                                  
         MVI   KEY+3,X'22'                                                      
*                                                                               
         MVC   KEY+10(6),SPACES                                                 
         MVC   KEY+10(4),QPUB+3    LIST CODE AND BUY NUM                        
*                                                                               
PP3      DS    0H                                                               
         GOTO1 HIGH                                                             
         B     PP3D                                                             
PP3B     DS    0H                                                               
         GOTO1 SEQ                                                              
PP3D     DS    0H                                                               
         CLC   KEY(16),KEYSAVE                                                  
         BNE   PP4                                                              
         CLC   KEY+19(2),PESTKEST                                               
         BNE   PP3B                                                             
         TM    KEY+25,X'80'        TEST DELETED                                 
         BNZ   PP3B                                                             
         B     PP6                                                              
*                                                                               
PP4      DS    0H                                                               
         MVC   P(27),=C'**PROTOTYPE BUY NOT FOUND**'                            
         MVC   PSECOND(80),QPROG                                                
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 APRNT                                                            
         MVI   MODE,LBUYREQ                                                     
         B     EXIT                                                             
*                                                                               
PP6      DS    0H                                                               
         MVI   FRST,X'FF'                                                       
         GOTO1 GETBUY                                                           
*                                                                               
         CLI   PBDSPACE,X'FF'      TEST SPECIAL OUTDOOR FIELDS                  
         BNE   PP10                                                             
         ZAP   PBDREG,=P'0'                                                     
         ZAP   PBDILLUM,=P'0'                                                   
PP10     DS    0H                                                               
         CLI   PBDSPACE,C'*'       IF HAVE SPACE                                
         BNH   PP11                                                             
         ZAP   PBDUNITS,=P'0'      CLEAR UNITS                                  
         ZAP   PBDCLMS,=P'0'                                                    
PP11     DS    0H                                                               
         ZAP   PBDPRCOS,=P'0'                                                   
         ZAP   PBDCOS,=P'0'                                                     
         ZAP   PBDCD,=P'0'                                                      
         ZAP   PBDACP,=P'0'                                                     
         MVI   PBDCOSIN,C' '                                                    
         XC    PBDCDATE,PBDCDATE                                                
         XC    PBDSDATE,PBDSDATE                                                
         XC    PBDBDATE,PBDBDATE                                                
         XC    PBDPDATE,PBDPDATE                                                
         MVC   PBDBUYDT,BTODAY                                                  
         XC    PBDDATE,PBDDATE                                                  
         MVI   PBDDTIND,0                                                       
         L     RF,ALISREC                                                       
         MVC   PBDLIST,PLISKCOD-PLISREC(RF)                                     
         MVI   PBDDTIN2,0                                                       
         MVI   PBDDTIN3,0                                                       
         XC    PBDCHGDT,PBDCHGDT                                                
         XC    PBDIODAT,PBDIODAT                                                
         XC    PBDIDAT2,PBDIDAT2                                                
         MVI   PBDEMIND,C' '                                                    
         MVI   PBUYKLIN,1               SET BUY LINE NO = 1                     
*                                                                               
         L     RF,ASVBUY           SAVE PROTOTYPE BUY                           
         LA    R1,1000                                                          
         MOVE  ((RF),(R1)),PBUYREC                                              
*                                                                               
         L     R5,ASVBUY                                                        
         BAS   RE,DMPREC                                                        
*                                                                               
*                                                                               
*                                  READ ALL APPLICABLE CONTRACTS                
PP20     DS    0H                                                               
         L     R4,ACONTAB                                                       
         MVI   0(R4),X'FF'                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(7),PCLTKEY                                                   
         MVI   KEY+3,X'10'                                                      
         MVC   KEY+07(6),PUBKPUB                                                
         GOTO1 HIGH                                                             
         B     PP21B                                                            
*                                                                               
PP21     DS    0H                                                               
         GOTO1 SEQ                                                              
PP21B    DS    0H                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PP22                                                             
         GOTO1 GETCONT                                                          
         CLC   PCONEDT,STADAT                                                   
         BL    PP21                                                             
         CLC   PCONSDT,ENDDAT                                                   
         BH    PP21                                                             
         MVC   HALF,PCONLEN             SAVE CONTRACT IN TABLE                  
         LH    R1,HALF                                                          
         MOVE  ((R4),(R1)),PCONREC                                              
         AH    R4,HALF                                                          
         MVI   0(R4),X'FF'                                                      
         B     PP21                                                             
*                                                                               
PP22     DS    0H                                                               
         LA    R6,DATLIST                                                       
PP23     DS    0H                                                               
         CLI   0(R6),X'FF'         END OF LIST                                  
         BE    PP30                                                             
*                                                                               
         L     R4,ACONTAB                                                       
         USING PCONREC,R4                                                       
PP24     DS    0H                                                               
         CLI   0(R4),X'FF'                                                      
         BE    PP25                                                             
         CLC   0(3,R6),PCONSDT                                                  
         BL    PP24B                                                            
         CLC   0(3,R6),PCONEDT                                                  
         BNH   PP25B                                                            
*                                                                               
PP24B    DS    0H                                                               
         MVC   HALF,PCONLEN                                                     
         AH    R4,HALF                                                          
         B     PP24                                                             
*                                                                               
PP25     DS    0H                                                               
         DROP  R4                                                               
         SR    R4,R4                                                            
PP25B    DS    0H                                                               
         L     RF,ASVBUY           MOVE PROTO TO BUYREC                         
         LA    R1,1000                                                          
         MOVE  (PBUYREC,(R1)),(RF)                                              
         GOTO1 ARTLOOK,DMCB,PBUYREC,PUBREC,(R4),BILLPROF,ADDAY                  
         MVC   ERRCD,DMCB                                                       
         MVI   DUPE,C'Y'                                                        
         MVC   PBUYKPUB,PUBKPUB                                                 
         MVC   KEY(25),PBUYKEY     TEST ALREADY ON FILE                         
         GOTO1 HIGH                                                             
         CLC   KEY(21),KEYSAVE                                                  
         BE    PP28                                                             
         MVI   DUPE,C'N'                                                        
*                                  ADD BUY                                      
         LA    RF,PBUYREC                                                       
         ST    RF,AREC                                                          
         CLI   RCWRITE,C'Y'                                                     
         BNE   PP28                                                             
         GOTO1 ADDPRT                                                           
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PP28     DS    0H                                                               
         GOTO1 HEXOUT,DMCB,CONNO,P+112,3,=C'N'                                  
         LA    R5,PBUYREC                                                       
         BAS   RE,DMPREC                                                        
*                                                                               
         LA    R6,3(R6)            NEXT DATE                                    
         B     PP23                                                             
*                                                                               
PP30     DS    0H                                                               
         B     EXIT                                                             
         SPACE 3                                                                
*        SET DATE LIST                                                          
SETDATS  NTR1                                                                   
*                                  READ CLT, PRD AND EST                        
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,2                                                          
         MVC   KEY+4(3),QCLIENT                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETCLI                                                           
*                                                                               
         MVC   KEY(7),PCLTKEY                                                   
         MVI   KEY+3,6                                                          
         MVC   KEY+7(3),QPRODUCT                                                
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETPROD                                                          
*                                                                               
         MVI   KEY+3,7                                                          
         PACK  DUB,QEST                                                         
         CVB   R0,DUB                                                           
         STCM  R0,3,KEY+10                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETEST                                                           
         GOTO1 DATCON,DMCB,QSTART,(3,DATLIST)                                   
         MVI   DATLIST+3,X'FF'                                                  
         MVC   ENDDAT,DATLIST                                                   
         XIT1                                                                   
         SPACE 3                                                                
*        UNDERLINE                                                              
ULINE    DS    0H                                                               
         LA    R1,P+30                                                          
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R0,P                                                             
         SR    R1,R0                                                            
         EX    R1,*+6                                                           
         BR    RE                                                               
         MVC   PSECOND(0),DASHES                                                
         SPACE 3                                                                
*        EDIT TOTALS                                                            
EDTOTS   DS    0H                                                               
         L     R2,0(R3)                                                         
         LA    RF,4                                                             
         LA    R4,P+30                                                          
ET4      DS    0H                                                               
         EDIT  (B4,0(R2)),(15,0(R4)),2,FLOAT=-                                  
         LA    R4,17(R4)                                                        
         LA    R2,4(R2)                                                         
         BCT   RF,ET4                                                           
*                                                                               
         BR    RE                                                               
         SPACE 3                                                                
*                                  ROLL UP TOTALS                               
ROLTOTS  DS    0H                                                               
         LR    R1,R3               FIND NEXT NEEDED LEVEL                       
         LA    R1,4(R1)                                                         
         CLI   0(R1),0                                                          
         BE    *-8                 NOT NEEDED                                   
         L     R2,0(R3)                                                         
         L     R1,0(R1)                                                         
         LA    R0,4                                                             
*                                                                               
RT4      DS    0H                                                               
         L     RF,0(R1)                                                         
         A     RF,0(R2)                                                         
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    R2,4(R2)                                                         
         BCT   R0,RT4                                                           
*                                                                               
         BR    RE                                                               
         SPACE 3                                                                
PRNT     NTR1                                                                   
         GOTO1 REPORT                                                           
         XIT1                                                                   
         SPACE 3                                                                
DMPREC   NTR1                                                                   
*                                  R5 POINTS TO REC                             
         MVC   HALF,25(R5)                                                      
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
         GOTO1 APRNT                                                            
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
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
*                                                                               
ACONS    DS    0F                                                               
         DC    V(RATELOOK)                                                      
         DC    A(CONTAB)                                                        
         DC    A(SVBUYR)                                                        
         DC    A(PRNT)                                                          
         DC    A(DSTTOTS)                                                       
         DC    A(REGTOTS)                                                       
         DC    A(REQTOTS)                                                       
         DC    A(RUNTOTS)                                                       
ACONSX   EQU   *                                                                
         SPACE 3                                                                
         EJECT                                                                  
*                                                                               
PP44WKD  DSECT                                                                  
RCONS    DS    0F                                                               
ARTLOOK  DS    A                                                                
ACONTAB  DS    A                                                                
ASVBUY   DS    A                                                                
APRNT    DS    A                                                                
*                                                                               
ATOTS    DS    0F                                                               
ADSTTOT  DS    A                                                                
AREGTOT  DS    A                                                                
AREQTOT  DS    A                                                                
ARUNTOT  DS    A                                                                
ATOTSN   EQU   (*-ATOTS)/4                                                      
*                                                                               
*                                                                               
FRST     DS    X                                                                
*                                                                               
STADAT   DS    0XL3                                                             
DATLIST  DS    105XL3                                                           
ENDDAT   DS    XL3                                                              
*                                                                               
CONNO    DS    X                                                                
ERRCD    DS    X                                                                
DUPE     DS    X                                                                
DASHES   DS    CL40'-'                                                          
BTODAY   DS    XL3                                                              
BILLPROF DS    XL3                                                              
         SPACE 3                                                                
PP44WC   CSECT                                                                  
*                                                                               
TOTS     DC    XL64'00'                                                         
         ORG   TOTS                                                             
DSTTOTS  DS    XL16                                                             
REGTOTS  DS    XL16                                                             
REQTOTS  DS    XL16                                                             
RUNTOTS  DS    XL16                                                             
*                                                                               
CONTAB   DS    5CL2000             5 CONTRACTS                                  
SVBUYR   DS    1000C                                                            
*                                                                               
         SPACE 3                                                                
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPMODEQU                                                       
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013PPREP4402 05/01/02'                                      
         END                                                                    
