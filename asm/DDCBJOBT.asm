*          DATA SET DDCBJOBT   AT LEVEL 002 AS OF 04/24/17                      
*PHASE CBJOBTA                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE CBJOB                                                                  
         TITLE 'CBJOB - TEST PROGRAM'                                           
         PRINT NOGEN                                                            
CBJOBT   CSECT                                                                  
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         NBASE WORKX-WORKD,**TEST**,=V(REGSAVE),RA,R9                           
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
*                                                                               
         L     R1,=A(IOAREA1-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIO1                                                          
         SHI   R1,8                                                             
         MVC   0(8,R1),=C'**IOA1**'                                             
*                                                                               
         L     R1,=A(IOAREA2-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIO2                                                          
         SHI   R1,8                                                             
         MVC   0(8,R1),=C'**IOA2**'                                             
*                                                                               
         USING PLINED,PLINE                                                     
*                                                                               
         BAS   RE,INIT             READ CARDS ECT                               
         BAS   RE,MAIN             MAIN LOOP                                    
         BAS   RE,DONE             CLOSE ALL                                    
*                                                                               
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE                                                                  
*                                                                               
EXITEQ   CR    RB,RB                                                            
         B     EXIT                                                             
EXITNE   LTR   RB,RB                                                            
EXIT     XIT1                                                                   
                                                                                
***********************************************************************         
* INITIALISE                                                                    
***********************************************************************         
INIT     NTR1                                                                   
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         ZAP   LINE,=P'0'                                                       
         ZAP   PAGE,=P'1'                                                       
         MVI   IWAITACT,0                                                       
         MVC   IWAITTIM,=F'0'                                                   
*                                                                               
         LA    R3,CARD                                                          
INIT010  GOTO1 VCARDS,DMCB,(R3),=C'RE00'                                        
         CLC   =C'DDSIO=',0(R3)                                                 
         BNE   INIT020                                                          
         L     RF,=V(DDSIO)        OVERRIDE DDSIO NAME                          
         MVC   0(8,RF),6(R3)                                                    
         B     INIT010                                                          
*                                                                               
INIT020  CLC   =C'DSPACE=',0(R3)                                                
         BNE   INIT030                                                          
         L     RF,=A(SSB)          SET DSPACE ID IN SSB                         
         MVC   SSODSPAC-SSOOFF(1,RF),7(R3)                                      
         B     INIT010                                                          
*                                                                               
INIT030  GOTO1 VDATCON,DMCB,(5,0),(2,TODAY)                                     
*                                                                               
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(15),=C'TEST PROGRAM  '                                     
         LA    R1,TITLE            PRINT PARAMETER CARDS TITLE                  
         BAS   RE,PRINTT                                                        
         LA    R3,CARD                                                          
         B     INIT051                                                          
*                                                                               
INIT050  GOTO1 VCARDS,DMCB,(R3),=C'RE00'                                        
INIT051  CLC   =C'/*',0(R3)                                                     
         BE    INIT200                                                          
         CLC   =C'XX',0(R3)                                                     
         BE    INIT200                                                          
         CLI   0(R3),C'*'                                                       
         BE    INIT050             SKIP COMMENTS                                
         MVC   PLINE+1(80),0(R3)                                                
         BAS   RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         CLC   =C'WRITE=',0(R3)                                                 
         BNE   INIT070                                                          
         CLC   6(3,R3),=C'YES'                                                  
         BNE   *+8                                                              
         MVI   RCWRITE,C'Y'        WRITE RECORD                                 
         B     INIT050                                                          
*                                                                               
INIT070  CLC   =C'DUMPREC=',0(R3)                                               
         BNE   INIT080                                                          
         MVC   RDUMP,8(R3)                                                      
         B     INIT050                                                          
*                                                                               
INIT080  CLC   =C'USERID=',0(R3)                                                
         BNE   INIT090                                                          
         MVC   RUSER,7(R3)                                                      
         B     INIT050                                                          
*                                                                               
INIT090  CLC   =C'JOB=',0(R3)                                                   
         BNE   INIT100                                                          
         MVC   RJOB,4(R3)                                                       
         B     INIT050                                                          
*                                                                               
INIT100  CLC   =C'DELAY=',0(R3)                                                 
         BNE   INIT110                                                          
         PACK  DUB,6(3,R3)                                                      
         CVB   RF,DUB                                                           
         MHI   RF,100              SECONDS                                      
         CLI   9(R3),C'S'                                                       
         BE    INIT102                                                          
         CLI   9(R3),C'M'                                                       
         JNE   *+2                                                              
         MHI   RF,60               MINUTES                                      
INIT102  STCM  RF,15,RDELAY        DELAY TIME                                   
         B     INIT050                                                          
*                                                                               
INIT110  CLC   =C'WAITACTION=',0(R3)                                            
         BNE   INIT120                                                          
         MVC   RWAITACT,11(R3)                                                  
         B     INIT050                                                          
*                                                                               
INIT120  CLC   =C'WAITTIME=',0(R3)                                              
         BNE   INIT130                                                          
         PACK  DUB,9(3,R3)                                                      
         CVB   RF,DUB                                                           
         MHI   RF,100                                                           
         CLI   12(R3),C'S'                                                      
         BE    INIT122                                                          
         CLI   12(R3),C'M'                                                      
         JNE   *+2                                                              
         MHI   RF,60               MINUTES                                      
INIT122  STCM  RF,15,RWAITTIM      NUMBER OF SECONDS TO WAIT                    
         B     INIT050                                                          
*                                                                               
INIT130  CLC   =C'ACTION=',0(R3)                                                
         BNE   INIT140                                                          
         LA    RE,ACTLIST                                                       
         LA    RF,L'ACTLIST                                                     
INIT131  CLI   0(RE),C' '          EMPTY ENTRY                                  
         BNH   INIT132                                                          
         LA    RE,1(,RE)                                                        
         BCT   RF,INIT131                                                       
         DC    H'0'                ACTION TABLE FULL                            
INIT132  MVC   0(1,RE),7(R3)       SAVE ACTION IN TABLE                         
         B     INIT050                                                          
*                                                                               
INIT140  CLC   =C'DSN=',0(R3)                                                   
         BNE   INIT150                                                          
         MVC   RDSN,4(R3)                                                       
         B     INIT050                                                          
*                                                                               
INIT150  B     INIT050                                                          
*                                                                               
INIT200  B     EXITEQ                                                           
                                                                                
***********************************************************************         
* READ DATA LINES                                                               
***********************************************************************         
MAIN     NTR1                                                                   
*                                                                               
         CLI   RDSN,C' '            USE REQUESTED DSN IF PROVIDED               
         BNH   *+10                                                             
         MVC   MYDSN,RDSN                                                       
*                                                                               
         LA    R2,ACTLIST                                                       
         LA    R3,L'ACTLIST                                                     
MAIN010  CLI   0(R2),C' '                                                       
         BNH   MAINX                                                            
*                                                                               
         MVC   PLIND,=C'---->'                                                  
*                                                                               
         SELECT CLI,0(R2),EQ                                                    
           WHEN (C'R')                                                          
             MVI   IACTION,CBJREG                                               
             MVC   IWAITACT,RWAITACT                                            
             MVC   IWAITTIM,RWAITTIM                                            
             MVC   PLMESS,=CL20'JOB REGISTER'                                   
             BRAS  RE,PRINTL                                                    
           WHEN (C'N')                                                          
             MVI   IACTION,CBJNREQ                                              
             MVC   PLMESS,=CL20'NEW REQUEST'                                    
             AP    PACKREQ,PACKONE                                              
             OI    PACKREQ+L'PACKREQ-1,X'0F'                                    
             UNPK  RREQ,PACKREQ                                                 
             BRAS  RE,PRINTL                                                    
           WHEN (C'W')                                                          
             MVI   IACTION,CBJWAIT                                              
             MVC   PLMESS,=CL20'ABOUT TO WAIT'                                  
             BRAS  RE,PRINTL                                                    
           WHEN (C'P')                                                          
             MVI   IACTION,CBJPOST                                              
             MVC   PLIND,=C'---->'                                              
             MVC   PLMESS,=CL20'ABOUT TO POST'                                  
             MVC   PLDSN,MYDSN                                                  
             BRAS  RE,PRINTL                                                    
           WHEN (C'C')                                                          
             MVI   IACTION,CBJCOMP                                              
             MVC   PLMESS,=CL20'JOB COMPLETED'                                  
             BRAS  RE,PRINTL                                                    
           WHEN (C'J')                                                          
             MVI   IACTION,CBJCLN                                               
             MVC   PLMESS,=CL20'ABOUT TO CLEAN'                                 
             BRAS  RE,PRINTL                                                    
           WHEN (C'H')                                                          
             MVC   PLMESS,=CL20'DELAY'                                          
             BRAS  RE,PRINTL                                                    
             STIMER WAIT,BINTVL=RDELAY                                          
             B     MAIN100                                                      
          ENDSEL                                                                
*                                                                               
          GOTO1 =V(CBJOB),DMCB,(IACTION,MYDSN),(RWAITACT,IWAITTIM),0            
*                                                                               
          MVC   BYTE,4(R1)                                                      
          MVC   PLRETURN,=C'RETURN CODE='                                       
          GOTO1 VHEXOUT,DMCB,BYTE,PLRETC,1                                      
          MVC   PLDSN,MYDSN                                                     
          BRAS  RE,PRINTL                                                       
*                                                                               
MAIN100  LA    R2,1(,R2)                                                        
         BCT   R3,MAIN010                                                       
*                                                                               
MAINX    B     EXITEQ                                                           
                                                                                
***********************************************************************         
* FINISHED PROCESSING                                                           
***********************************************************************         
DONE     NTR1                                                                   
         CLOSE SYSPRINT            CLOSE PRINT                                  
         B     EXITEQ                                                           
                                                                                
***********************************************************************         
*        PRINT REPORT                                                           
***********************************************************************         
PRTREP   NTR1                                                                   
         BAS   RE,PRINTL                                                        
PREX     B     EXITEQ                                                           
                                                                                
***********************************************************************         
*        PRINT ROUTINES                                                         
***********************************************************************         
PRINTT   ST    RE,SAVERE           PRINT TITLES                                 
         ZAP   LINE,=P'0'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,TITLE      PRINT TITLE                                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTL   ST    RE,SAVERE           PRINT LINE                                   
         AP    LINE,=P'1'          BUMP LINECOUNT                               
         CP    LINE,MAXLINE        TEST FOR MAX LINES                           
         BL    PRINTL2                                                          
*                                                                               
PRINTL1  ZAP   LINE,=P'3'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
*                                                                               
PRINTL2  PUT   SYSPRINT,PLINE      PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
         EJECT                                                                  
                                                                                
***********************************************************************         
*        CONSTANTS & LTORG                                                      
***********************************************************************         
VDATAMGR DC    V(DATAMGR)                                                       
VCARDS   DC    V(CARDS)                                                         
VPERVAL  DC    V(PERVAL)                                                        
VHEXOUT  DC    V(HEXOUT)                                                        
VHEXIN   DC    V(HEXIN)                                                         
VHELLO   DC    V(HELLO)                                                         
VDATCON  DC    V(DATCON)                                                        
VADDAY   DC    V(ADDAY)                                                         
VQSORT   DC    V(QSORT)                                                         
VSECRET  DC    V(SECRET)                                                        
VLOCKSPC DC    V(LOCKSPC)                                                       
*                                                                               
DMREAD   DC    CL8'DMREAD '                                                     
DMRDHI   DC    CL8'DMRDHI '                                                     
DMRSEQ   DC    CL8'DMRSEQ '                                                     
DMWRT    DC    CL8'DMWRT  '                                                     
SPACES   DC    166C' '                                                          
DASHES   DC    166C'-'                                                          
MAXLINE  DC    P'60'                                                            
*                                                                               
RCWRITE  DC    C'N'                                                             
RDUMP    DC    C'N'                                                             
RWAITACT DC    C'O'                                                             
RWAITTIM DC    AL4(15*60*100)                                                   
RDELAY   DC    AL4(1*60*100)                                                    
TIMEIT   DC    AL4(15*60*100)                                                   
*                                                                               
ACTLIST  DC    CL100' '                                                         
*                                                                               
MYDSN    DS    0CL44                                                            
         DC    C'CMDEVFTP'                                                      
         DC    C'.TST'                                                          
         DC    C'.ID0'                                                          
RUSER    DC    C'00000'                                                         
         DC    C'.JOB'                                                          
RJOB     DC    C'00000'                                                         
         DC    C'.R'                                                            
RREQ     DC    C'000'                                                           
         DC    C'.RSP'                                                          
         DC    CL(L'MYDSN-(*-MYDSN))' ' ENFORCE LENGTH OF DSN                   
*                                                                               
PACKREQ  DC    PL2'0'                                                           
PACKONE  DC    PL1'1'                                                           
*                                                                               
RDSN     DC    CL44' '                                                          
*                                                                               
MERROR   DC    CL10'**ERROR***'                                                 
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
*        DCBS & ADCONS                                                          
***********************************************************************         
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
*                                                                               
         DC    0D                                                               
         DC    CL16'*UTL*UTL*UTL*UTL'                                           
UTL      DC    F'0',X'0A',XL3'00',XL252'00'                                     
         DC    0D                                                               
         DC    CL16'*SSB*SSB*SSB*SSB'                                           
SSB      DC    X'0000'                                                          
         DC    X'FF'                                                            
         DC    AL1(SSOSNRCV)                                                    
         DC    1024X'00'                                                        
*                                                                               
       ++INCLUDE FATABSDEQU                                                     
*                                                                               
         DS    16D                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
*        WORKING STORAGE                                                        
***********************************************************************         
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
MAINRD   DS    A                                                                
SAVERE   DS    A                                                                
*                                                                               
AIO1     DS    A                   A(IO AREA 1)                                 
AIO2     DS    A                   A(IO AREA 2)                                 
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
DMCB     DS    6F                                                               
PLIST    DS    6F                                                               
WORK     DS    CL64                                                             
MYWORK   DS    CL64                                                             
LINE     DS    PL2                                                              
PAGE     DS    PL4                                                              
*                                                                               
*                                                                               
PLINE    DS    CL166                                                            
TITLE    DS    CL166                                                            
*                                                                               
TODAY    DS    XL2                 TODAY                                        
*                                                                               
CARD     DS    CL80                                                             
KEY      DS    CL40                                                             
         DS    CL40                                                             
*                                                                               
ELEM     DS    XL256               GENERAL USE ELEMENT                          
*                                                                               
IACTION  DS    C                                                                
IWAITACT DS    C                                                                
IWAITTIM DS    F                                                                
*                                                                               
         DS    CL8                                                              
IOAREA1  DS    2048C               IO AREA 1                                    
         DS    CL8                                                              
IOAREA2  DS    2048C               IO AREA 1                                    
*                                                                               
SPARE    DS    1024X                                                            
WORKX    EQU   *                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* PRINT LINE DSECTS                                                             
***********************************************************************         
PLINED   DSECT                                                                  
PLIND    DS    CL5                                                              
PLMESS   DS    CL20                                                             
         DS    CL2                                                              
PLRETURN DS    CL12                                                             
PLRETC   DS    CL2                                                              
         DS    CL6                                                              
PLDSN    DS    CL44                                                             
*                                                                               
         DCBD    DSORG=QS,DEVD=DA                                               
*                                                                               
       ++INCLUDE DDCBJOBD                                                       
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
                                                                                
SSBOFFD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DDCBJOBT  04/24/17'                                      
         END                                                                    
