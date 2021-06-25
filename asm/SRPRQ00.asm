*          DATA SET SRPRQ00    AT LEVEL 008 AS OF 01/13/10                      
*PHASE T13300A                                                                  
         TITLE '$PRQ - DISPLAY REMOTE PRINTER DATA'                             
         PRINT NOGEN                                                            
PRQ      CSECT                                                                  
         NMOD1 WORKL,**$PRQ**,CLEAR=YES                                         
         USING WORKD,RC            RC=A(W/S)                                    
*                                                                               
         USING SRPARMD,R1                                                       
         L     RA,SRQATWA          RA=A(TWA)                                    
         USING SRPRQFFD,RA                                                      
         L     R9,SRQASYSF         R9=A(SYSFACS)                                
         USING SYSFACD,R9                                                       
*                                                                               
         L     RE,SRQACOMF                                                      
         USING COMFACSD,RE                                                      
         MVC   VTERMVAL,CTERMVAL                                                
         MVC   VHEXOUT,CHEXOUT                                                  
         DROP  R1,RE                                                            
*                                                                               
         THMS                                                                   
         ST    R1,TIMENOW          TIME NOW P'0HHMMSS+'                         
         BRAS  RE,ON31                                                          
*                                                                               
P1VAL    LA    R2,SRVP1H           P1 IS TERMINAL ID                            
         USING FHD,R2                                                           
*                                                                               
         L     R5,VUTL                                                          
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING UTLD,R5             R5=A(UTL)                                    
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,1,FHIL                                                        
         BZ    P2VAL               LUID NOT INPUT                               
*                                                                               
         TM    FHII,FHIINU         TEST LUID IS NUMERIC                         
         BO    P1V04               YES                                          
*                                                                               
         LA    RF,FHDA                                                          
P1V02    CLI   0(RF),C'*'          TEST IF ANY WILD CARD CHRS IN UTL            
         BNE   *+12                                                             
         OI    TRMFILT,X'80'       SET WILD CARD FLAG                           
         B     P2VAL                                                            
*                                                                               
         LA    RF,1(RF)                                                         
         BCT   R1,P1V02                                                         
         CLI   FHIL,6              TEST FULL LUID INPUT                         
         BH    P1V04                                                            
         OI    TRMFILT,X'40'       SET PART TERM FLAG                           
         B     P2VAL                                                            
*                                                                               
P1V04    GOTO1 VTERMVAL,DMCB,(R2)                                               
         ICM   R5,15,DMCB+4        R5 = A(UTL)                                  
         BZ    ERR1                                                             
*                                                                               
P2VAL    LA    R2,SRVP2H           P2 IS FILTERS                                
         CLI   FHIL,1                                                           
         BL    P3VAL                                                            
         BH    ERR2                                                             
*                                                                               
         MVC   STAFILT,FHDA        SET FILTER                                   
*                                                                               
         CLI   STAFILT,C'A'                                                     
         BE    P3VAL                                                            
         CLI   STAFILT,C'W'                                                     
         BE    P3VAL                                                            
         CLI   STAFILT,C'S'                                                     
         BE    P3VAL                                                            
         CLI   STAFILT,C'I'                                                     
         BE    P3VAL                                                            
         CLI   STAFILT,C'E'                                                     
         BE    P3VAL                                                            
         CLI   STAFILT,C'Q'        QUEUE ERRORS                                 
         BE    P3VAL                                                            
         CLI   STAFILT,C'T'        ANY TIME TODAY                               
         BE    P3VAL                                                            
*                                                                               
         CLI   STAFILT,C'P'        PRINTING IS ACTV                             
         BNE   ERR2                                                             
         MVI   STAFILT,C'A'                                                     
*                                                                               
P3VAL    LA    R2,SRVP3H           P3 IS P=PRINTER/S=SHUTTLE                    
         CLI   FHIL,1                                                           
         BL    DIS                                                              
         BH    ERR2                                                             
*                                                                               
         MVC   TYPFILT,FHDA                                                     
         CLI   TYPFILT,C'P'                                                     
         BE    DIS                                                              
         CLI   TYPFILT,C'S'                                                     
         BE    DIS                                                              
         B     ERR2                                                             
         EJECT                                                                  
*                                                                               
DIS      LA    R2,SRVL1H           R2=A(TWA DISPLAY LINE)                       
         USING LINED,R2                                                         
*                                                                               
DIS02    XR    R4,R4                                                            
         ICM   R4,15,TXPRNT        TEST IS THIS IS A PRINTER DEVICE             
         BZ    DIS44                                                            
         USING PRQD,R4             R4=A(PRINTER QUEUE HEADER)                   
*                                                                               
         XC    LUID,LUID                                                        
         LA    RE,NUMPRTS          POINT TO PRINTER COUNTS                      
         TM    TTYPE,TTYPERMC                                                   
         BZ    *+8                                                              
         LA    RE,NUMSHTS          POINT TO SHUTTLE COUNTS                      
         L     RF,0(RE)                                                         
         AHI   RF,1                                                             
         ST    RF,0(RE)                                                         
*                                                                               
         XR    R1,R1                                                            
         IC    R1,PRQNE                                                         
         L     RF,4(RE)                                                         
         AR    RF,R1                                                            
         ST    RF,4(RE)                                                         
*                                                                               
         CLI   DISPMODE,0          BUMP TO NEXT IF NOT DISPLAYING               
         BNE   DIS44                                                            
*                                                                               
DIS04    CLI   TRMFILT,0           ANY TERMINAL FILTERING                       
         BE    DIS12               NO                                           
         TM    TRMFILT,X'01'       OK IF HAVE FOUND FIRST QUALIFIER             
         BO    DIS12                                                            
*                                                                               
         MVC   LUID,TLUID                                                       
*                                                                               
         LA    RF,SRVP1            RF=A(INPUT LUID)                             
         SR    R1,R1               R1=L'INPUT LUID                              
         ICM   R1,1,SRVP1H+FHIL-FHD                                             
         BZ    DIS12                                                            
*                                                                               
         TM    TRMFILT,X'80'       WILD CARD                                    
         BO    DIS06                                                            
         TM    TRMFILT,X'40'       SHORT FORM                                   
         BO    DIS10                                                            
         B     DIS12                                                            
*                                                                               
DIS06    LA    RE,LUID             WILD CARD MATCHING                           
*                                                                               
DIS08    CLI   0(RF),C'*'                                                       
         BE    *+14                                                             
         CLC   0(1,RF),0(RE)                                                    
         BNE   DIS44                                                            
*                                                                               
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,DIS08                                                         
         B     DIS12                                                            
*                                                                               
DIS10    BCTR  R1,0                SHORT LUID MATCHING                          
         EX    R1,*+8                                                           
         BNE   DIS44                                                            
         CLC   LUID(0),0(RF)                                                    
         OI    TRMFILT,X'01'       SET WE HAVE FOUND FIRST MATCH                
*                                                                               
DIS12    CLI   STAFILT,0           ANY STATUS FILTERING                         
         BE    DIS18               NO                                           
         CLI   STAFILT,C'Q'        TEST QUEUE ERRORS                            
         BNE   DIS13                                                            
         CLI   PNQOK,0                                                          
         BNE   DIS18                                                            
         B     DIS44                                                            
*                                                                               
DIS13    CLI   STAFILT,C'T'        ANY TIME TODAY                               
         BNE   DIS14                                                            
         OC    PRSVHMS,PRSVHMS                                                  
         BZ    DIS14                                                            
         L     RF,NUMTODAY                                                      
         AHI   RF,1                                                             
         ST    RF,NUMTODAY                                                      
         B     DIS18                                                            
*                                                                               
DIS14    OC    PRCIADDR,PRCIADDR                                                
         BNZ   DIS16                                                            
         CLI   STAFILT,C'I'        TEST INACTIVE                                
         BE    DIS18                                                            
         B     DIS44                                                            
*                                                                               
DIS16    CLI   STAFILT,C'I'        TEST INACTIVE                                
         BE    DIS44                                                            
         CLI   STAFILT,C'W'        TEST ACTIVE (PNTG/SNDG)                      
         BNE   *+18                                                             
         OC    TCID,TCID                                                        
         BZ    DIS18                                                            
         B     DIS44                                                            
*                                                                               
         CLI   STAFILT,C'A'        TEST ACTIVE (PNTG/SNDG)                      
         BE    DIS18                                                            
         TM    PRSTAT,PRSACTV                                                   
         BO    DIS44                                                            
         CLI   STAFILT,C'S'        TEST STOPPED                                 
         BE    DIS18                                                            
         CLI   STAFILT,C'E'        TEST ERROR                                   
         BNE   DIS18                                                            
         TM    PRSTAT,PRSERR                                                    
         BZ    DIS44                                                            
*                                                                               
DIS18    CLI   TYPFILT,0           ANY TYPE FILTERING                           
         BE    DIS22               NO                                           
         CLI   TYPFILT,C'S'        SHUTTLES ONLY                                
         BNE   DIS20                                                            
         TM    TTYPE,TTYPERMC                                                   
         BO    DIS22                                                            
         B     DIS44                                                            
*                                                                               
DIS20    CLI   TYPFILT,C'P'        PRINTERS ONLY                                
         BNE   DIS22                                                            
         TM    TTYPE,TTYPERMC                                                   
         BZ    DIS22                                                            
         B     DIS44                                                            
*                                                                               
DIS22    LH    R1,TNUM             TERMINAL NUMBER                              
         CVD   R1,DUB                                                           
         UNPK  LINNUM,DUB                                                       
         OI    LINNUM+3,X'F0'                                                   
         MVC   LINSYM,TSYM         LUID                                         
*                                                                               
DIS26    MVI   LINTYPE,C'P'        PRINTER TYPE                                 
         TM    TTYPE,TTYPERMC                                                   
         BZ    *+8                                                              
         MVI   LINTYPE,C'S'                                                     
         MVI   LINTYPE+1,C','                                                   
         LA    R3,LINTYPE+2        PRINTER SPEED                                
         TM    PRQSPDTY,X'20'      TEST IF OLD CHR MODE REPRESENTATION          
         BO    DIS28               NO                                           
         SR    R0,R0               YES SPEED WAS 2 BYTES LONG                   
         ICM   R0,3,PRQSPD-1                                                    
         EDIT  (R0),(4,0(R3)),ALIGN=LEFT,ZERO=NOBLANK                           
         AR    R3,R0                                                            
         MVC   0(3,R3),=C'CPS'                                                  
         CLI   PRQSPDTY,C'C'       B=C=CHRS PER SEC                             
         BNH   DIS32                                                            
         CLI   PRQSPDTY,C'L'       L=LINES PER MINUTE                           
         BNE   *+10                                                             
         MVC   0(3,R3),=C'LPM'                                                  
         CLI   PRQSPDTY,C'P'       P=PAGES PER MINUTE                           
         BNE   *+10                                                             
         MVC   0(3,R3),=C'PPM'                                                  
         B     DIS32                                                            
*                                                                               
DIS28    SR    R0,R0               YES SPEED NOW ONE CHR LONG                   
         ICM   R0,1,PRQSPDTY       AND SPEED TYPE IN TOP TWO BITS               
         SRL   R0,6                                                             
         STC   R0,BYTE             0=P/MIN,1=L/6SECS,2=3=C/HALFSEC              
         ICM   R0,1,PRQSPD                                                      
         CLI   BYTE,0              TEST PAGES PER MIN                           
         BE    DIS30                                                            
         CLI   BYTE,1              TEST LINES PER 6SEC                          
         BNE   *+12                                                             
         MHI   R0,10               CONVERT TO LINES PER MINUTE                  
         B     DIS30                                                            
         SLL   R0,1                CONVERT TO CHARS PER SEC                     
*                                                                               
DIS30    EDIT  (R0),(4,0(R3)),ALIGN=LEFT,ZERO=NOBLANK                           
         AR    R3,R0                                                            
         SR    R1,R1                                                            
         IC    R1,BYTE                                                          
         SLL   R1,2                                                             
         LA    R1,DSPD(R1)                                                      
         MVC   0(3,R3),0(R1)                                                    
         B     DIS32                                                            
*                                                                               
DSPD     DC    C'PPM LPM BPS CPS'                                               
*                                                                               
DIS32    LH    R1,PRQBUFFL         BUFFER SIZE                                  
         CVD   R1,DUB                                                           
         UNPK  LINSIZE,DUB                                                      
         OI    LINSIZE+3,X'F0'                                                  
         CLI   PNQOK,0                                                          
         BE    DIS34                                                            
         MVC   LINSIZE,=C'*  *'                                                 
         GOTO1 VHEXOUT,DMCB,PNQOK,LINSIZE+1,1,=C'TOG'                           
*                                                                               
DIS34    ST    R5,DUB              A(UTL)                                       
         GOTO1 VHEXOUT,DMCB,DUB,LINAUTL,4,0                                     
         MVC   DUB(4),TXPRNT                                                    
         GOTO1 (RF),(R1),,LINAPRQ  A(PRQ)                                       
*                                                                               
         OC    PRSVHMS,PRSVHMS     LAST TIME ACTIVITY                           
         BZ    DIS36                                                            
         MVC   WORK(10),=X'402120204B20204B2020'                                
         ED    WORK(10),PRSVHMS                                                 
         MVC   LINTIME,WORK+2                                                   
*                                                                               
DIS36    MVC   LINSTAT,LINACTV     PRINTER STATUS                               
         OC    PRCIADDR,PRCIADDR                                                
         BZ    DIS42                                                            
         MVC   LINSTAT,LPNTG       PRINTING                                     
         TM    TTYPE,TTYPERMC                                                   
         BZ    *+10                                                             
         MVC   LINSTAT,LSNDG       SENDING                                      
         TM    PRSTAT,PRSACTV                                                   
         BO    DIS38                                                            
         MVC   LINSTAT,LESTOP      ERROR STOPPED                                
         TM    PRSTAT,PRSERR                                                    
         BO    *+14                                                             
         MVC   LINSTAT,LUSTOP      USER SPOPPED                                 
         B     DIS38                                                            
         TM    PRSTAT2,PRS2PATH+PRS2UNLG                                        
         BNO   *+14                                                             
         MVC   LINSTAT,LLOLG       LOST SESSION AND UNABLE TO LOGON             
         B     DIS38                                                            
         TM    PRSTAT2,PRS2PATH                                                 
         BZ    *+14                                                             
         MVC   LINSTAT,LLOST       LOST SESSION DUE TO PATH ERROR               
         B     DIS38                                                            
         TM    PRSTAT2,PRS2UNLG                                                 
         BZ    *+14                                                             
         MVC   LINSTAT,LUNLG       UNABLE TO LOGON                              
         B     DIS38                                                            
         TM    PRSTAT3,PRS3FTS                                                  
         BZ    *+14                                                             
         MVC   LINSTAT,LFTS        FAILURE TO START (OPENDEST)                  
         B     DIS38                                                            
*                                                                               
DIS38    MVC   DUB,PR1KEY          EXTRACT PRTQ REPORT KEY                      
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'            BUILD KEY OF NUMERIC ID RECORD               
         MVC   KEY+23(2),DUB                                                    
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'CTFILE',KEY,REC,TO24=Y               
         BRAS  RE,ON31                                                          
         CLI   8(R1),0                                                          
         BNE   ERR4                                                             
         LA    R1,REC+28                                                        
         SR    RE,RE                                                            
DIS40    CLI   0(R1),0             LOOK FOR ALPHA-ID ELEMENT                    
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),X'02'                                                      
         BE    *+14                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     DIS40                                                            
*                                                                               
         MVC   LINREPID(10),2(R1)  MOVE ID TO FIELD                             
         LA    R1,LINREPID+9                                                    
         CLI   0(R1),C' '          FIND END OF ID                               
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C','          DELIMIT                                      
         MVC   2(3,R1),DUB+2       DOP ON SUB-ID                                
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '          2CHR SUB-ID                                  
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         MVI   1(R1),C','          DELIMIT                                      
         LA    R3,2(R1)                                                         
         MVC   DUB(2),DUB+5                                                     
         LH    R1,DUB              REPORT NUMBER                                
         EDIT  (R1),(4,0(R3)),ALIGN=LEFT                                        
*                                                                               
DIS42    LA    R2,LINNEXT(R2)      BUMP TO NEXT TWA LINE                        
         CLI   0(R2),0                                                          
         BNE   *+8                                                              
         MVI   DISPMODE,1          SET NOT DISPLAYING - END OF SCREEN           
*                                                                               
DIS44    BXLE  R5,R6,DIS02         BUMP TO NEXT UTL ENTRY                       
         B     OKEXIT                                                           
         EJECT                                                                  
* ERRORS AND EXIT                                                               
*                                                                               
ERR1     MVC   MSG(23),=C'INVALID TERMINAL FORMAT'                              
         B     ERRX                                                             
ERR2     MVC   MSG(19),=C'INVALID INPUT FIELD'                                  
         B     ERRX                                                             
ERR3     MVC   MSG(20),=C'DISK ERROR ON PRTQUE'                                 
         B     ERRX                                                             
ERR4     MVC   MSG(21),=C'ERROR ON CONTROL FILE'                                
         B     ERRX                                                             
ERRX     NI    SRVSRVH+6,X'BF'                                                  
         XC    SRVMSG,SRVMSG                                                    
         OI    6(R2),X'40'                                                      
         MVC   SRVMSG(11),=C'** ERROR **'                                       
         MVC   SRVMSG+12(40),MSG                                                
         B     EXIT                                                             
*                                                                               
OKEXIT   MVC   WORK(10),=X'402120204B20204B2020'                                
         ED    WORK(10),TIMENOW                                                 
         MVC   SRVMSG+43(8),WORK+2                                              
*                                                                               
OKEXIT0  ICM   RF,15,NUMTODAY      NUM ACTIVE TODAY                             
         BZ    OKEXIT0X                                                         
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FULL,DUB+4(4)                                                    
         MVC   SRVMSG+52(4),=C'Tod='                                            
         MVC   SRVMSG+56(4),FULL                                                
OKEXIT0X EQU   *                                                                
*                                                                               
OKEXIT1  ICM   RF,15,NUMPRTS       NUM PRINTERS AND AVG ENTRIES                 
         BZ    OKEXIT1X                                                         
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FULL,DUB+4(4)                                                    
         MVC   SRVMSG+05(4),FULL                                                
         L     R1,NUMPRTES                                                      
         M     R0,=F'10'                                                        
         DR    R0,RF                                                            
         EDIT  (R1),(4,FULL),1                                                  
         OC    FULL(4),=C'00.0'                                                 
         MVC   SRVMSG+15(4),FULL                                                
OKEXIT1X EQU   *                                                                
*                                                                               
OKEXIT2  ICM   RF,15,NUMSHTS       NUM SHUTTLES AND AVG ENTRIES                 
         BZ    OKEXIT2X                                                         
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FULL,DUB+4(4)                                                    
         MVC   SRVMSG+26(4),FULL                                                
         L     R1,NUMSHTES                                                      
         M     R0,=F'10'                                                        
         DR    R0,RF                                                            
         EDIT  (R1),(4,FULL),1                                                  
         OC    FULL(4),=C'00.0'                                                 
         MVC   SRVMSG+36(4),FULL                                                
OKEXIT2X B     EXIT                                                             
*                                                                               
ON31     O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
*                                                                               
OFF31    N     RE,=X'7FFFFFFF'                                                  
         BSM   0,RE                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
LINACTV  DC    CL6'INACTV'                                                      
LPNTG    DC    CL6'A/PNTG'                                                      
LSNDG    DC    CL6'A/SNDG'                                                      
LESTOP   DC    CL6'E/STOP'                                                      
LLOLG    DC    CL6'E/LOLG'                                                      
LLOST    DC    CL6'E/LOST'                                                      
LUNLG    DC    CL6'E/UNLG'                                                      
LFTS     DC    CL6'E/FTS '                                                      
LUSTOP   DC    CL6'S/USER'                                                      
*                                                                               
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* FAPQREC                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAPQREC                                                        
         PRINT ON                                                               
* FAPRQ                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAPRQ                                                          
         PRINT ON                                                               
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
VTERMVAL DS    V                                                                
VHEXOUT  DS    V                                                                
DMCB     DS    6F                                                               
*                                                                               
NUMCNTS  DS    0CL16                                                            
NUMPRTS  DS    F                   NUMBER OF PRINTERS                           
NUMPRTES DS    F                   NUMBER OF PRINTER QUEUE ENTRIES              
NUMSHTS  DS    F                   NUMBER OF SHUTTLES                           
NUMSHTES DS    F                   NUMBER OF PRINTER QUEUE ENTRIES              
NUMTODAY DS    F                   NUMBER ACTIVE TODAY                          
*                                                                               
TIMENOW  DS    PL4                                                              
LUID     DS    CL8                                                              
TRMFILT  DS    C                                                                
STAFILT  DS    C                                                                
TYPFILT  DS    C                                                                
DISPMODE DS    X                                                                
MSG      DS    CL60                                                             
KEY      DS    CL25                                                             
WORK     DS    CL20                                                             
REC      DS    6144C                                                            
WORKL    EQU   *-WORKD                                                          
         SPACE 2                                                                
LINED    DSECT                                                                  
         DS    CL8                                                              
LINNUM   DS    CL4                                                              
         DS    C                                                                
LINSYM   DS    CL8                                                              
         DS    C                                                                
LINTYPE  DS    CL9                                                              
         DS    C                                                                
LINSIZE  DS    CL4                                                              
         DS    C                                                                
LINAUTL  DS    CL8                                                              
         DS    C                                                                
LINAPRQ  DS    CL8                                                              
         DS    C                                                                
LINTIME  DS    CL5                                                              
         DS    C                                                                
LINSTAT  DS    CL6                                                              
         DS    C                                                                
LINREPID DS    CL18                                                             
         DS    C                                                                
LINNEXT  EQU   *-LINED                                                          
         EJECT                                                                  
SRPRQFFD DSECT                                                                  
         DS    CL64                                                             
* SRPRQFFD                                                                      
       ++INCLUDE SRPRQFFD                                                       
* DDFH                                                                          
       ++INCLUDE DDFH                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SRPRQ00   01/13/10'                                      
         END                                                                    
