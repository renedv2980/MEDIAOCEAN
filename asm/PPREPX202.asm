*          DATA SET PPREPX202  AT LEVEL 013 AS OF 05/01/02                      
*PHASE PPX202A,+0                                                               
*INCLUDE IJFFZZWZ                                                               
         TITLE 'PPREPX202 - PRINTPAK COKE EXTRACT'                              
PPX202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPX202                                                         
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PPX2WKD,R8                                                       
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         CLI   MODE,FBUYREQ                                                     
         BE    REQF                                                             
         CLI   MODE,PROCBUY                                                     
         BE    PRBUY                                                            
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
*        RUN FIRST                                                              
         SPACE 2                                                                
RUNF     DS    0H                                                               
*                                  RELOCATE ACONS                               
         LA    R0,(ACONSX-ACONS)/4                                              
         LA    R2,ACONS                                                         
         LA    R3,RCONS                                                         
         RELOC                                                                  
RUNF2    DS    0H                                                               
         L     RF,0(R2)                                                         
         AR    RF,RE                                                            
         ST    RF,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,RUNF2                                                         
*                                                                               
         MVC   APRNT,REPORT                                                     
         B     EXIT                                                             
         SPACE 2                                                                
*        REQ FIRST                                                              
         SPACE 2                                                                
REQF     DS    0H                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,X'FD'                                                   
         CLC   =C'ALL',QEST                                                     
         BNE   *+10                                                             
         MVC   QEST,SPACES                                                      
         MVC   SVQSTRT(12),QSTART                                               
         MVC   QSTART(12),SPACES                                                
         GOTO1 DATCON,DMCB,SVQSTRT,(3,BQS)                                      
         GOTO1 (RF),(R1),SVQEND,(3,BQE)                                         
*                                                                               
REQF4    DS    0H                                                               
         CLI   QOPT2,C'Y'          TEST NEED PUB TAPE                           
         BE    REQF5                                                            
         CLI   QOPT2,C'P'                                                       
         BNE   REQF6                                                            
*                                                                               
REQF5    DS    0H                                                               
         BAS   RE,PPROC                                                         
*                                                                               
REQF6    DS    0H                                                               
         CLI   QOPT1,C'N'          TEST NEED BUY TAPE                           
         BNE   REQF6B                                                           
         MVI   MODE,LBUYREQ                                                     
         B     EXIT                                                             
*                                                                               
REQF6B   DS    0H                                                               
         CLI   BLOPSW,C'Y'                                                      
         BE    REQF8                                                            
         MVI   BLOPSW,C'Y'                                                      
*&&DO                                                                           
         L     R1,APPX2BL                                                       
         OPENR (1)                                                              
*&&                                                                             
*&&OS                                                                           
         L     R2,APPX2BL                                                       
         OPEN  ((2),OUTPUT)                                                     
*&&                                                                             
*                                                                               
REQF8    DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        RUN LAST                                                               
         SPACE 2                                                                
RUNL     DS    0H                                                               
         MVI   SPACING,2                                                        
         MVC   P,SPACES                                                         
         GOTO1 APRNT                                                            
         MVC   P(17),=C'**REPORT TOTALS**'                                      
         GOTO1 APRNT                                                            
         LA    R3,TWDS                                                          
         LA    R2,TGRS                                                          
         LA    R4,4                                                             
*                                                                               
RUNL4    DS    0H                                                               
         MVC   P(10),0(R3)                                                      
         EDIT  (B4,0(R2)),(16,P+11),2,COMMAS=YES,CR=YES                         
         GOTO1 APRNT                                                            
*                                                                               
         LA    R2,4(R2)                                                         
         LA    R3,10(R3)                                                        
         BCT   R4,RUNL4                                                         
*                                                                               
         CLI   BLOPSW,C'Y'                                                      
         BNE   RUNL6                                                            
         L     R2,APPX2BL                                                       
*&&DO                                                                           
         CLOSER (2)                                                             
*&&                                                                             
*&&OS                                                                           
         CLOSE ((2),)                                                           
*&&                                                                             
RUNL6    DS    0H                                                               
         CLI   PBOPSW,C'Y'                                                      
         BNE   RUNL8                                                            
         L     R2,APPX2PB                                                       
*&&DO                                                                           
         CLOSER (2)                                                             
*&&                                                                             
*&&OS                                                                           
         CLOSE ((2),)                                                           
*&&                                                                             
*                                                                               
RUNL8    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*        PROC BUY                                                               
         SPACE 2                                                                
PRBUY    DS    0H                                                               
         LA    R4,IPREC                                                         
         USING IPBILLD,R4                                                       
         MVI   IPREC,C' '                                                       
         MVC   IPBILL+1(L'IPBILL-1),IPBILL                                      
         LA    R2,PBUYREC+33                                                    
PB4      DS    0H                                                               
         MVI   ELCODE,X'26'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   PB40                                                             
         ST    R2,SAVR2                                                         
*                                                                               
         USING PBILELEM,R2                                                      
         CLC   PBLDATE,BQS                                                      
         BL    PB4                                                              
         CLC   PBLDATE,BQE                                                      
         BH    PB4                                                              
*                                                                               
         ZIC   RF,PBLDATE+1                                                     
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  IPBBMON,DUB                                                      
         ICM   RF,3,PBINVNO                                                     
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  IPBINUM,DUB                                                      
*                                                                               
         ICM   RF,15,PBGROSS                                                    
         CVD   RF,DUB                                                           
         UNPK  IPBGRS,DUB                                                       
         L     R1,TGRS                                                          
         AR    R1,RF                                                            
         ST    R1,TGRS                                                          
*                                                                               
         ICM   RE,15,PBCSHDSC                                                   
         CVD   RE,DUB                                                           
         UNPK  IPBCD,DUB                                                        
         L     R1,TCD                                                           
         AR    R1,RE                                                            
         ST    R1,TCD                                                           
*                                                                               
         ICM   R0,15,PBAGYCOM                                                   
         CVD   R0,DUB                                                           
         UNPK  IPBCOMM,DUB                                                      
         L     R1,TCOMM                                                         
         AR    R1,R0                                                            
         ST    R1,TCOMM                                                         
*                                                                               
         SR    RF,RE                                                            
         SR    RF,R0                                                            
         CVD   RF,DUB                                                           
         UNPK  IPBNET,DUB                                                       
         L     R1,TNET                                                          
         AR    R1,RF                                                            
         ST    R1,TNET                                                          
*                                                                               
         MVC   IPBPRD,PBPRD                                                     
*                                                                               
         CLI   IPBILL,C' '         TEST REC INITIALIZED                         
         BNE   PB20                                                             
*                                                                               
         MVI   IPBMED,C'P'                                                      
         MVC   IPBMED+1(1),QMEDIA                                               
         MVC   IPBAGY,QAGENCY                                                   
         MVC   IPBCLT,PBUYKCLT                                                  
         MVC   IPBCNUM,PCLTNUM                                                  
*                                                                               
         MVC   IPBCAT,PBUYKCLT+2        CATEGORY IS LAST CLT CHAR               
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBDBDATE),WORK                                    
         MVC   IPBMOS(2),WORK+2    BILL MONTH                                   
         MVC   IPBMOS+2(2),WORK    YEAR                                         
         BAS   RE,FLZONE           FLOAT IN ZONE NAME                           
         MVC   IPBNAME,PFNAME                                                   
*                                  GET REG/DST ASSIGNMENT                       
         L     R2,ALTLREC                                                       
         LA    R2,33(R2)                                                        
         MVI   ELCODE,X'71'                                                     
         CLC   ELCODE,0(R2)                                                     
         BE    PB13A                                                            
PB13     DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   PB13B                                                            
PB13A    DS    0H                                                               
         USING PUBDSTEL,R2                                                      
         CLC   PUBDCLT,PBUYKCLT                                                 
         BNE   PB13                                                             
*                                                                               
         MVC   IPBMNUM,PUBDREG+2   LAST OF REG AND 3 OF DIST                    
         B     PB14                                                             
*                                                                               
PB13B    DS    0H                                                               
         MVC   P+63(29),=C'NO REGION/DISTRICT ASSIGNMENT'                       
         BAS   RE,PRTERR                                                        
*                                                                               
*                                  GET CLIENT VENDOR NUM                        
PB14     DS    0H                                                               
         LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'14'                                                     
PB15     DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   PB15B                                                            
         USING PUBREPEL,R2                                                      
         CLI   PUBRPOFF,X'FF'     USE OFFICE CLIENT VENDOR NUM                  
         BNE   PB15                                                             
         CLC   PUBRPOFF+1(1),PCLTOFF                                            
         BNE   PB15                                                             
         CLI   PUBCVEN,C' '                                                     
         BH    PB15D                                                            
*                                                                               
PB15B    DS    0H                                                               
         MVC   P+63(14),=C'NO COKE NUMBER'                                      
         BAS   RE,PRTERR                                                        
         B     PB16                                                             
PB15D    DS    0H                                                               
         MVC  IPBPUB(4),PUBCVEN                                                 
*                                                                               
PB16     DS    0H                                                               
         MVC   IPBPNUM,PUBKPUB     SET PUB CODE                                 
*                                                                               
PB20     DS    0H                                                               
*                                                                               
         CLI   QOPT3,C'Y'          TEST TO PRINT                                
         BNE   PB26                                                             
         MVC   P(100),IPREC                                                     
         MVC   PSECOND(50),IPREC+100                                            
         GOTO1 APRNT                                                            
*                                                                               
PB26     DS    0H                                                               
         L     R1,APPX2BL                                                       
         LA    R0,IPREC                                                         
         PUT   (1),(0)                                                          
*                                                                               
         L     R2,SAVR2            NEXT ELEMENT                                 
         B     PB4                                                              
*                                                                               
PB40     DS    0H                                                               
         B     EXIT                                                             
*                                                                               
PRTERR   NTR1                                                                   
         GOTO1 PUBEDIT,DMCB,PUBKPUB,(C'S',P)                                    
         MVC   P+20(41),PFNAME                                                  
         MVC   P+100(2),=C'**'                                                  
         GOTO1 APRNT                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                  CREATE PUB TAPE                              
PPROC    NTR1                                                                   
*                                  SET BINSRCH PARAMS                           
         SR    R0,R0                                                            
         L     R1,APLST                                                         
         SR    R2,R2                                                            
         LA    R3,6                                                             
         LA    R4,6                                                             
         LH    R5,=Y(6000)                                                      
         STM   R0,R5,BSPARS                                                     
*                                                                               
         CLI   PBOPSW,C'Y'                                                      
         BE    PPR2                                                             
         MVI   PBOPSW,C'Y'                                                      
*&&DO                                                                           
         L     R1,APPX2PB                                                       
         OPENR (1)                                                              
*&&                                                                             
*&&OS                                                                           
         L     R2,APPX2PB                                                       
         OPEN  ((2),OUTPUT)                                                     
*&&                                                                             
*                                                                               
PPR2     DS    0H                                                               
*                                                                               
         LA    R2,CLLIST                                                        
         LA    R3,CLN                                                           
         CLI   QOPT2,C'P'          TEST DOING ALL CLIENTS                       
         BNE   PPR4                                                             
PPR4     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY                                                   
         MVI   KEY+3,X'21'                                                      
         CLI   QOPT2,C'P'          TEST DOING ALL CLIENTS                       
         BE    *+10                                                             
         MVC   KEY+4(3),0(R2)                                                   
PPR5     DS    0H                                                               
         GOTO1 HIGH                                                             
         B     PPR6B                                                            
PPR6     DS    0H                                                               
         GOTO1 SEQ                                                              
PPR6B    DS    0H                                                               
         CLC   KEY(4),KEYSAVE      A/M/REC                                      
         BNE   PPR12                                                            
         CLC   KEY(7),KEYSAVE      CLT                                          
         BNE   PPR10                                                            
*                                                                               
         GOTO1 BINSRCH,BSPARS,(1,KEY+7)                                         
         MVI   KEY+13,X'FF'        NEXT PUB                                     
         B     PPR5                                                             
PPR10    DS    0H                                                               
         CLI   QOPT2,C'P'          TEST DOING ALL CLIENTS                       
         BE    PPR10D                                                           
         LA    R2,3(R2)            NEXT CLT                                     
         BCT   R3,PPR4                                                          
         B     PPR12                                                            
*                                                                               
PPR10D   DS    0H                                                               
         MVC   KEYSAVE(7),KEY      NEXT CLT                                     
         B     PPR5                                                             
*                                                                               
*                                                                               
PPR12    DS    0H                                                               
         OC    QPROG+52(4),=4C'0'                                               
         PACK  PSQ,QPROG+52(4)                                                  
*                                                                               
         L     R7,BSPARS+8         NO. OF PUBS                                  
         LTR   R7,R7                                                            
         BNP   PPRX                                                             
         L     R6,BSPARS+4         A(FIRST)                                     
*                                                                               
PPR14    DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),QMEDIA                                                    
         MVC   KEY+1(6),0(R6)                                                   
         MVC   KEY+7(2),QAGENCY                                                 
         MVI   KEY+9,X'81'                                                      
*                                                                               
         GOTO1 HIGHPUB                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   PPR36                                                            
         GOTO1 GETNAME                                                          
*                                                                               
         LA    R4,IPREC                                                         
         USING IPPUBD,R4                                                        
         MVI   IPPUB,C' '                                                       
         MVC   IPPUB+1(L'IPPUB-1),IPPUB                                         
*                                                                               
         MVI   IPPMED,C'P'                                                      
         MVC   IPPMED+1(1),QMEDIA                                               
         MVC   IPPAGY,QAGENCY                                                   
*                                                                               
         UNPK  IPPPUB(4),PSQ                                                    
         OI    IPPPUB+3,X'F0'                                                   
*                                                                               
         BAS   RE,FLZONE           FLOAT IN ZONE NAME                           
         MVC   IPPNAM,PFNAME                                                    
         MVC   IPPSTR,PUBLINE1                                                  
         MVC   IPPCTY(16),PUBCITY                                               
         MVC   IPPST(2),PUBSTATE                                                
         MVC   IPPZIP,PUBZIP                                                    
*                                                                               
         GOTO1 PUBEDIT,DMCB,(8,PUBKPUB),(C'S',IPPPNUM)                          
*                                                                               
         L     R1,APPX2PB                                                       
         LA    R0,IPREC                                                         
         PUT   (1),(0)                                                          
*                                                                               
         CLI   QOPT3,C'Y'                                                       
         BNE   PPR20                                                            
         MVC   P(100),IPREC                                                     
         MVC   PSECOND(50),IPREC+100                                            
         GOTO1 APRNT                                                            
*                                                                               
PPR20    DS    0H                                                               
*                                  PUT ASSIGNED NUMBER IN CLIENT ELEM           
         LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'14'                                                     
PPR22    DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   PPR26                                                            
         USING PUBREPEL,R2                                                      
         CLI   PUBRPOFF,X'FF'                                                   
         BNE   PPR22                                                            
         CLI   PUBRPOFF+1,C'S'   TEST RIGHT OFFICE                              
         BNE   PPR22                                                            
PPR24    MVC   PUBCVEN,SPACES                                                   
         UNPK  PUBCVEN(4),PSQ                                                   
         OI    PUBCVEN+3,X'F0'                                                  
         B     PPR28                                                            
*                                                                               
PPR26    DS    0H                                                               
         LR    R4,R2                                                            
         LA    R2,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   PUBREPEL(2),=X'1420'                                             
         MVI   PUBRPOFF,X'FF'                                                   
         MVI   PUBRPOFF+1,C'S'       OFFICE CODE                                
         MVI   PUBRPOFF+2,C' '                                                  
         MVC   PUBCVEN,SPACES                                                   
         UNPK  PUBCVEN(4),PSQ                                                   
         OI    PUBCVEN+3,X'F0'                                                  
         GOTO1 RECUP,DMCB,(X'01',PUBREC),(R2),(R4)                              
*                                                                               
PPR28    DS    0H                                                               
PPR30    DS    0H                                                               
         LA    RF,PUBREC                                                        
         ST    RF,AREC                                                          
         CLI   RCWRITE,C'Y'                                                     
         BNE   PPR34                                                            
         GOTO1 PUTPUB                                                           
PPR34    DS    0H                                                               
         AP    PSQ,=P'1'                                                        
PPR36    DS    0H                                                               
         LA    R6,6(R6)            NEXT PUB                                     
         BCT   R7,PPR14                                                         
*                                                                               
PPRX     XIT1                                                                   
         SPACE 3                                                                
NEXTEL   DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
NEXTEL2  DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
FLZONE   DS    0H                                                               
         MVC   PFNAME,SPACES                                                    
         MVC   PFNAME(20),PUBNAME                                               
         LA    RF,PFNAME+20                                                     
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   2(20,RF),PUBZNAME                                                
         BR    RE                                                               
*                                                                               
         SPACE 3                                                                
*                                                                               
CLLIST   DC    C'CCACCBCCICCUFCC'                                               
CLN      EQU   (*-CLLIST)/3                                                     
*                                                                               
MASTCLT  DC    C'CC '                                                           
*                                                                               
TWDS     DS    0C                                                               
         DC    CL10'GROSS'                                                      
         DC    CL10'CASH DISC'                                                  
         DC    CL10'NET'                                                        
         DC    CL10'COMMISSION'                                                 
*                                                                               
ACONS    DS    0F                                                               
         DC    A(PPX2BL)                                                        
         DC    A(PPX2PB)                                                        
         DC    A(PLST)                                                          
ACONSX   EQU   *                                                                
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
         SPACE 3                                                                
*&&DO                                                                           
PPX2BL   DTFMT DEVADDR=SYS008,BLKSIZE=150,RECFORM=FIXUNB,              X        
               TYPEFLE=OUTPUT,WORKA=YES,IOAREA1=OUTA,FILABL=STD                 
*&&                                                                             
*&&OS                                                                           
PPX2BL   DCB   DDNAME=PPX2BL,          DOS SYS008                      X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00150,                                            X        
               BLKSIZE=00150,          DOS BLKSIZE=00150               X        
               MACRF=PM                                                         
*&&                                                                             
*                                                                               
*&&DO                                                                           
PPX2PB   DTFMT DEVADDR=SYS009,BLKSIZE=150,RECFORM=FIXUNB,              X        
               TYPEFLE=OUTPUT,WORKA=YES,IOAREA1=OUTA,FILABL=STD                 
*&&                                                                             
*&&OS                                                                           
PPX2PB   DCB   DDNAME=PPX2PB,          DOS SYS009                      X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00150,                                            X        
               BLKSIZE=00150,          DOS BLKSIZE=00150               X        
               MACRF=PM                                                         
*&&                                                                             
*                                                                               
*&&DO                                                                           
OUTA     DS    150C                                                             
*&&                                                                             
*                                                                               
PLST     DS    36000C                                                           
         SPACE 3                                                                
PPX2WKD  DSECT                                                                  
RCONS    DS    0C                                                               
APPX2BL  DS    A                                                                
APPX2PB  DS    A                                                                
APLST    DS    A                                                                
*                                                                               
APRNT    DS    A                                                                
*                                                                               
BQS      DS    XL3                                                              
BQE      DS    XL3                                                              
PSQ      DS    PL3                                                              
PBOPSW   DS    X                                                                
BLOPSW   DS    X                                                                
SVQSTRT  DS    CL6                                                              
SVQEND   DS    CL6                                                              
ELCODE   DS    XL1                                                              
PFNAME   DS    CL41                                                             
SAVR2    DS    F                                                                
BSPARS   DS    6F                                                               
*                                                                               
TGRS     DS    F                                                                
TCD      DS    F                                                                
TNET     DS    F                                                                
TCOMM    DS    F                                                                
*                                                                               
IPREC    DS    CL150                                                            
*                                                                               
IPBILLD  DSECT                                                                  
IPBILL   DS    0CL150                                                           
IPBMED   DS    CL2                                                              
IPBAGY   DS    CL2                                                              
IPBCLT   DS    CL3                                                              
IPBCNUM  DS    CL4                                                              
IPBPRD   DS    CL3                                                              
         DS    CL5                                                              
         DS    CL3                                                              
IPBCAT   DS    CL1                                                              
IPBMNUM  DS    CL4                                                              
         DS    CL7                                                              
IPBPUB   DS    CL4                                                              
         DS    CL3                                                              
IPBNAME  DS    CL30                                                             
IPBMOS   DS    CL4                                                              
IPBINUM  DS    CL4                                                              
IPBBMON  DS    CL2                                                              
         DS    CL19                                                             
IPBGRS   DS    CL10                                                             
IPBCD    DS    CL10                                                             
IPBNET   DS    CL10                                                             
IPBCOMM  DS    CL10                                                             
IPBPNUM  DS    XL6                                                              
         DS    CL4                                                              
*                                                                               
*                                                                               
IPPUBD   DSECT                                                                  
IPPUB    DS    0CL150                                                           
IPPMED   DS    CL2                                                              
IPPAGY   DS    CL2                                                              
IPPPUB   DS    CL7                                                              
IPPNAM   DS    CL20                                                             
IPPSTR   DS    CL24                                                             
IPPCTY   DS    CL24                                                             
IPPST    DS    CL3                                                              
IPPZIP   DS    CL5                                                              
*                                                                               
IPPPNUM  DS    CL15                                                             
         DS    CL20                                                             
*                                                                               
         DS    CL28                                                             
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPMODEQU                                                       
*                                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013PPREPX202 05/01/02'                                      
         END                                                                    
