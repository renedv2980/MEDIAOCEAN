*          DATA SET SRPRQ00S   AT LEVEL 003 AS OF 08/22/00                      
*PHASE T13300A                                                                  
         TITLE '$PRQ - DISPLAY REMOTE PRINTER DATA'                             
         PRINT NOGEN                                                            
PRQ      CSECT                                                                  
         NMOD1 WORKX-WORKD,**$PRQ**                                             
         USING WORKD,RC            RC=A(W/S)                                    
         USING SRPARMD,R1                                                       
         L     RA,SRPARM6                                                       
         USING SRPRQFFD,RA         RA=A(TWA)                                    
         L     RE,SRPARM4                                                       
         USING COMFACSD,RE                                                      
         MVC   VTERMVAL,CTERMVAL                                                
         MVC   VHEXOUT,CHEXOUT                                                  
         XC    MSG,MSG                                                          
         DROP  RE                                                               
         L     R9,SRPARM1          R9=A(SYSFACS)                                
         USING SYSFACD,R9                                                       
         DROP  R1                                                               
         THMS                                                                   
         ST    R1,TIMENOW          TIME NOW P'0HHMMSS+'                         
         SPACE 2                                                                
P1VAL    LA    R2,SRVP1H           P1 IS TERMINAL ID                            
         L     R5,VUTL                                                          
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING UTLD,R5             R5=A(UTL)                                    
         MVI   TRMFILT,0                                                        
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)                                                       
         BZ    P1VALX              LUID NOT INPUT                               
         TM    4(R2),X'08'                                                      
         BO    P1V2                LUID IS NUMERIC                              
         LA    RF,8(R2)                                                         
P1V1     CLI   0(RF),C'*'          TEST IF ANY WILD CARD CHRS IN UTL            
         BNE   *+12                                                             
         OI    TRMFILT,X'80'       SET WILD CARD FLAG                           
         B     P1VALX                                                           
         LA    RF,1(RF)                                                         
         BCT   R1,P1V1                                                          
         CLI   5(R2),6                                                          
         BH    *+12                                                             
         OI    TRMFILT,X'40'       SET PART TERM FLAG                           
         B     P1VALX                                                           
P1V2     GOTO1 VTERMVAL,DMCB,(R2)                                               
         L     R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    ERR1                                                             
P1VALX   EQU   *                                                                
         SPACE 2                                                                
P2VAL    MVI   STAFILT,0           A=ACTV/I=INACTIVE,E=ERROR,S=STOPPED          
         LA    R2,SRVP2H                                                        
         CLI   5(R2),1                                                          
         BL    P2VALX                                                           
         BH    ERR2                                                             
         MVC   STAFILT,8(R2)                                                    
         CLI   STAFILT,C'A'                                                     
         BE    P2VALX                                                           
         CLI   STAFILT,C'S'                                                     
         BE    P2VALX                                                           
         CLI   STAFILT,C'I'                                                     
         BE    P2VALX                                                           
         CLI   STAFILT,C'E'                                                     
         BE    P2VALX                                                           
         CLI   STAFILT,C'P'        PRINTING IS ACTV                             
         BNE   *+12                                                             
         MVI   STAFILT,C'A'                                                     
         B     P2VALX                                                           
         CLI   STAFILT,C'Q'        QUEUE ERRORS                                 
         BE    P2VALX                                                           
         B     ERR2                                                             
P2VALX   EQU   *                                                                
         SPACE 2                                                                
P3VAL    MVI   TYPFILT,0           P3 IS P=PRINTER/S=SHUTTLE                    
         LA    R2,SRVP3H                                                        
         CLI   5(R2),1                                                          
         BL    P3VALX                                                           
         BH    ERR2                                                             
         MVC   TYPFILT,8(R2)                                                    
         CLI   TYPFILT,C'P'                                                     
         BE    P3VALX                                                           
         CLI   TYPFILT,C'S'                                                     
         BE    P3VALX                                                           
         B     ERR2                                                             
P3VALX   EQU   *                                                                
         EJECT                                                                  
         LA    R2,SRVL1H           R2=A(TWA DISPLAY LINE)                       
         USING LINED,R2                                                         
         XC    NUMCNTS,NUMCNTS     INITIALISE COUNTS                            
         MVI   DISPMODE,0          SET DISPLAYING DATA                          
*                                                                               
PR0      SR    R4,R4               TEST IS THIS IS A PRINTER DEVICE             
         ICM   R4,7,TPRNT                                                       
         BZ    PR14                                                             
         USING PRQD,R4             R4=A(PRINTER QUEUE HEADER)                   
         XC    LUID,LUID                                                        
         LA    RE,NUMPRTS          POINT TO PRINTER COUNTS                      
         TM    TTYPE,TTYPERMC                                                   
         BZ    *+8                                                              
         LA    RE,NUMSHTS          POINT TO SHUTTLE COUNTS                      
         L     RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         ST    RF,0(RE)                                                         
         SR    R1,R1                                                            
         IC    R1,PRQNE                                                         
         L     RF,4(RE)                                                         
         AR    RF,R1                                                            
         ST    RF,4(RE)                                                         
         CLI   DISPMODE,0          BUMP TO NEXT IF NOT DISPLAYING               
         BNE   PR14                                                             
*                                                                               
PR1      CLI   TRMFILT,0           ANY TERMINAL FILTERING                       
         BE    PR1X                NO                                           
         TM    TRMFILT,X'01'       OK IF HAVE FOUND FIRST QUALIFIER             
         BO    PR1X                                                             
         GETLA (R5),LUID,ADDR=ALPHA                                             
         LA    RF,SRVP1            RF=A(INPUT LUID)                             
         SR    R1,R1               R1=L'INPUT LUID                              
         IC    R1,SRVP1H+5                                                      
         TM    TRMFILT,X'80'       WILD CARD                                    
         BO    PR1A                                                             
         TM    TRMFILT,X'40'       SHORT FORM                                   
         BO    PR1B                                                             
         B     PR1X                                                             
PR1A     LA    RE,LUID             WILD CARD MATCHING                           
         LTR   R1,R1                                                            
         BZ    PR1X                                                             
PR1A1    CLI   0(RF),C'*'                                                       
         BE    *+14                                                             
         CLC   0(1,RF),0(RE)                                                    
         BNE   PR14                                                             
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,PR1A1                                                         
         B     PR1X                                                             
PR1B     BCTR  R1,0                SHORT LUID MATCHING                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   LUID(0),0(RF)                                                    
         BNE   PR14                                                             
         OI    TRMFILT,X'01'       SET WE HAVE FOUND FIRST MATCH                
         B     PR1X                                                             
PR1X     EQU   *                                                                
*                                                                               
PR2      CLI   STAFILT,0           ANY STATUS FILTERING                         
         BE    PR2X                NO                                           
         CLI   STAFILT,C'Q'        TEST QUEUE ERRORS                            
         BNE   *+16                                                             
         CLI   PNQOK,0                                                          
         BNE   PR2X                                                             
         B     PR14                                                             
         OC    PRCIADDR,PRCIADDR                                                
         BNZ   PR2A                                                             
         CLI   STAFILT,C'I'        TEST INACTIVE                                
         BE    PR2X                                                             
         B     PR14                                                             
PR2A     CLI   STAFILT,C'I'        TEST INACTIVE                                
         BE    PR14                                                             
         CLI   STAFILT,C'A'        TEST ACTIVE (PNTG/SNDG)                      
         BE    PR2X                                                             
         TM    PRSTAT,PRSACTV                                                   
         BO    PR14                                                             
         CLI   STAFILT,C'S'        TEST STOPPED                                 
         BE    PR2X                                                             
         CLI   STAFILT,C'E'        TEST ERROR                                   
         BNE   PR2X                                                             
         TM    PRSTAT,PRSERR                                                    
         BZ    PR14                                                             
PR2X     EQU   *                                                                
*                                                                               
PR3      CLI   TYPFILT,0           ANY TYPE FILTERING                           
         BE    PR3X                NO                                           
PR3A     CLI   TYPFILT,C'S'        SHUTTLES ONLY                                
         BNE   PR3B                                                             
         TM    TTYPE,TTYPERMC                                                   
         BO    PR3X                                                             
         B     PR14                                                             
PR3B     CLI   TYPFILT,C'P'        PRINTERS ONLY                                
         BNE   PR3X                                                             
         TM    TTYPE,TTYPERMC                                                   
         BZ    PR3X                                                             
         B     PR14                                                             
PR3X     EQU   *                                                                
         SPACE 2                                                                
PR6      LH    R1,TNUM             TERMINAL NUMBER                              
         CVD   R1,DUB                                                           
         UNPK  LINNUM,DUB                                                       
         OI    LINNUM+3,X'F0'                                                   
*                                                                               
         OC    LUID,LUID                                                        
         BNZ   PR7                                                              
         GETLA (R5),LUID,ADDR=ALPHA                                             
PR7      MVC   LINSYM,LUID                                                      
*                                                                               
         MVI   LINTYPE,C'P'        PRINTER TYPE                                 
         TM    TTYPE,TTYPERMC                                                   
         BZ    *+8                                                              
         MVI   LINTYPE,C'S'                                                     
         MVI   LINTYPE+1,C','                                                   
         LA    R3,LINTYPE+2        PRINTER SPEED                                
         EDIT  (B2,PRQSPD),(4,0(R3)),ALIGN=LEFT,ZERO=NOBLANK                    
         AR    R3,R0                                                            
         MVC   0(3,R3),=C'LPM'                                                  
         CLI   PRQSPDTY,C'L'                                                    
         BE    *+10                                                             
         MVC   0(3,R3),=C'CPS'                                                  
*                                                                               
         LH    R1,PRQBUFFL         BUFFER SIZE                                  
         CVD   R1,DUB                                                           
         UNPK  LINSIZE,DUB                                                      
         OI    LINSIZE+3,X'F0'                                                  
         CLI   PNQOK,0                                                          
         BE    PR7A                                                             
         MVC   LINSIZE,=C'*  *'                                                 
         GOTO1 VHEXOUT,DMCB,PNQOK,LINSIZE+1,1,=C'TOG'                           
*                                                                               
PR7A     ST    R5,DUB              A(UTL)                                       
         GOTO1 VHEXOUT,DMCB,DUB+1,LINAUTL,3,=C'TOG'                             
         MVC   DUB+1(3),TPRNT                                                   
*                                                                               
         GOTO1 (RF),(R1),,LINAPRQ  A(PRQ)                                       
*                                                                               
         OC    PRSVHMS,PRSVHMS     LAST TIME ACTIVITY                           
         BZ    PR8                                                              
         MVC   WORK(10),=X'402120204B20204B2020'                                
         ED    WORK(10),PRSVHMS                                                 
         MVC   LINTIME,WORK+2                                                   
*                                                                               
PR8      MVC   LINSTAT,LINACTV     PRINTER STATUS                               
         OC    PRCIADDR,PRCIADDR                                                
         BZ    PR12                                                             
         MVC   LINSTAT,LPNTG       PRINTING                                     
         TM    TTYPE,TTYPERMC                                                   
         BZ    *+10                                                             
         MVC   LINSTAT,LSNDG       SENDING                                      
         TM    PRSTAT,PRSACTV                                                   
         BO    PR9                                                              
         MVC   LINSTAT,LESTOP      ERROR STOPPED                                
         TM    PRSTAT,PRSERR                                                    
         BO    *+14                                                             
         MVC   LINSTAT,LUSTOP      USER SPOPPED                                 
         B     PR9                                                              
         TM    PRSTAT2,PRS2PATH+PRS2UNLG                                        
         BNO   *+14                                                             
         MVC   LINSTAT,LLOLG       LOST SESSION AND UNABLE TO LOGON             
         B     PR9                                                              
         TM    PRSTAT2,PRS2PATH                                                 
         BZ    *+14                                                             
         MVC   LINSTAT,LLOST       LOST SESSION DUE TO PATH ERROR               
         B     PR9                                                              
         TM    PRSTAT2,PRS2UNLG                                                 
         BZ    *+14                                                             
         MVC   LINSTAT,LUNLG       UNABLE TO LOGON                              
         B     PR9                                                              
         TM    PRSTAT3,PRS3FTS                                                  
         BZ    *+14                                                             
         MVC   LINSTAT,LFTS        FAILURE TO START (OPENDEST)                  
         B     PR9                                                              
*                                                                               
PR9      MVC   DUB,PR1KEY          EXTRACT PRTQ REPORT KEY                      
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'            BUILD KEY OF NUMERIC ID RECORD               
         MVC   KEY+23(2),DUB                                                    
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'CTFILE',KEY,REC                      
         CLI   8(R1),0                                                          
         BNE   ERR4                                                             
         LA    R1,REC+28                                                        
         SR    RE,RE                                                            
PR10     CLI   0(R1),0             LOOK FOR ALPHA-ID ELEMENT                    
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),X'02'                                                      
         BE    *+14                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     PR10                                                             
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
PR12     LA    R2,LINNEXT(R2)      BUMP TO NEXT TWA LINE                        
         CLI   0(R2),0                                                          
         BNE   *+8                                                              
         MVI   DISPMODE,1          SET NOT DISPLAYING - END OF SCREEN           
*                                                                               
PR14     BXLE  R5,R6,PR0           BUMP TO NEXT UTL ENTRY                       
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
OKEXIT2X EQU   *                                                                
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
LINACTV  DC    CL8'INACTIVE'                                                    
LPNTG    DC    CL8'ACT PNTG'                                                    
LSNDG    DC    CL8'ACT SNDG'                                                    
LESTOP   DC    CL8'ERR STOP'                                                    
LLOLG    DC    CL8'ERR LOLG'                                                    
LLOST    DC    CL8'ERR LOST'                                                    
LUNLG    DC    CL8'ERR UNLG'                                                    
LFTS     DC    CL8'ERR FTS '                                                    
LUSTOP   DC    CL8'STOP USR'                                                    
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
VTERMVAL DS    V                                                                
VHEXOUT  DS    V                                                                
DMCB     DS    6F                                                               
*                                                                               
NUMCNTS  DS    0CL16                                                            
NUMPRTS  DS    F                   NUMBER OF PRINTERS                           
NUMPRTES DS    F                   NUMBER OF PRINTER QUEUE ENTRIES              
NUMSHTS  DS    F                   NUMBER OF SHUTTLES                           
NUMSHTES DS    F                   NUMBER OF PRINTER QUEUE ENTRIES              
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
WORKX    EQU   *                                                                
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
LINAUTL  DS    CL6                                                              
         DS    C                                                                
LINAPRQ  DS    CL6                                                              
         DS    C                                                                
LINTIME  DS    CL8                                                              
         DS    C                                                                
LINSTAT  DS    CL8                                                              
         DS    C                                                                
LINREPID DS    CL18                                                             
LINNEXT  EQU   *-LINED                                                          
         EJECT                                                                  
SRPRQFFD DSECT                                                                  
         DS    CL64                                                             
* SRPRQFFD                                                                      
       ++INCLUDE SRPRQFFD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SRPRQ00S  08/22/00'                                      
         END                                                                    
