*          DATA SET DDMQPUT1   AT LEVEL 051 AS OF 08/17/04                      
*PHASE MQPUT1A                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'READ A DATASET AND MQPUT IT ALL IN ONE MESSAGE'                 
         MACRO                                                                  
&NAME    PRNT  &A,&PRINT=                                                       
&NAME    DS    0H                                                               
         AIF   (T'&A EQ 'O').NOMOVE                                             
         MVC   P(17),=CL17'&A'                                                  
.NOMOVE  ANOP                                                                   
         TIME  DEC                                                              
         ST    R0,PRNTDUB                                                       
         MVI   PRNTDUB+4,X'0F'                                                  
         UNPK  PRNTTIME,PRNTDUB(5)                                              
         MVC   P+18(2),PRNTTIME                                                 
         MVI   P+20,C':'                                                        
         MVC   P+21(2),PRNTTIME+2                                               
         MVI   P+23,C':'                                                        
         MVC   P+24(2),PRNTTIME+4                                               
         MVI   P+26,C'.'                                                        
         MVC   P+27(2),PRNTTIME+6                                               
         AIF   ('&PRINT' EQ 'ALWAYS').SKIP                                      
         CLI   TRACEFLG,C'Y'                                                    
         BNE   *+10                                                             
.SKIP    L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         MEND                                                                   
         EJECT                                                                  
MQPUT1   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,MQPUT1,=A(R13CHAIN)                                            
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         BAS   RE,INITIAL          INITIALIZE                                   
*                                                                               
         LA    R2,CSQBCONN_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     CONNECT TO MQ QUEUE MANAGER                  
*                                                                               
         OPEN  FILEIN                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R8,AMQMAIL                                                       
         XC    BUFFERLENGTH,BUFFERLENGTH    LENGTH OF DATA                      
*                                                                               
GETREC   XC    RECORD,RECORD                                                    
         GET   FILEIN,RECORD                                                    
*                                                                               
         LA    RF,L'RECORD                                                      
         LA    R1,RECORD+L'RECORD-1   POINT TO LAST CHARACTER IN RECORD         
         CLI   0(R1),C' '          FIND LAST SIGNIFICANT CHARACTER              
         BH    *+10                                                             
         BCTR  R1,0                                                             
         BCT   RF,*-10                                                          
*                                                                               
         L     RE,BUFFERLENGTH                                                  
         AR    RE,RF                                                            
         ST    RE,BUFFERLENGTH     LENGTH OF DATA                               
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),RECORD      MOVE DATA TO MQ BUFFER AREA                  
*                                                                               
         AHI   RF,1                                                             
         AR    R8,RF                                                            
*                                                                               
         C     R8,AEOMBUFF                                                      
         BNH   GETREC                                                           
         DC    H'0'                EXCESS THE LIMIT OF MAIL BUFFER              
*                                                                               
GOODBYE  EQU   *                                                                
         MVC   OBJDESC_OBJECTTYPE,=A(MQOT_Q)  OBJECT IS A QUEUE                 
         LA    RF,MQPMO_SYNCPOINT                                               
         ICM   RE,15,=AL4(MQPMO_FAIL_IF_QUIESCING)                              
         AR    RF,RE                                                            
         ST    RF,PUTMSGOPTS_OPTIONS                                            
*                                                                               
         LA    R2,CSQBPUT1_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     PUT THE MESSAGE TO THE MQ QUEUE              
*                                                                               
         LA    R2,CSQBDISC_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     DISCONNECT FROM MQ QUEUE MANAGER             
*                                                                               
         CLOSE FILEIN                                                           
*                                                                               
         PRNT  EXITING,PRINT=ALWAYS                                             
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
INITIAL  NTR1                                                                   
*                                                                               
* INITIALIZE                                                                    
*                                                                               
         BLDL  0,ENTRYPTS            BUILD LIST OF APPC/MVS ENTRY PTS           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                  BAD RETURN FROM BLDL MACRO                 
*                                                                               
         LOAD  DE=CSQBCONN                                                      
         ST    R0,CSQBCONN_STRUCTURE                                            
         LOAD  DE=CSQBPUT1                                                      
         ST    R0,CSQBPUT1_STRUCTURE                                            
         LOAD  DE=CSQBDISC                                                      
         ST    R0,CSQBDISC_STRUCTURE                                            
*                                                                               
* OBTAIN STORAGE FOR E-MAIL BUFFER                                              
*                                                                               
         L     R3,=F'4194304'      MAX MESSAGE SIZE                             
         AHI   R3,8                ROOM FOR EYE-CATCHER                         
*                                                                               
         STORAGE OBTAIN,LENGTH=(3)                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL STORAGE OBTAIN                  
         MVC   0(8,R1),=C'*MQMAIL*'                                             
         LA    R1,8(R1)            BUMP PAST LABEL                              
         ST    R1,AMQMAIL          A(MQ MAIL BUFFER)                            
         A     R1,=F'4194304'                                                   
         ST    R1,AEOMBUFF         A(END OF MQ MAIL BUFFER)                     
*                                                                               
         LA    R1,CSQBPUT1_STRUCTURE                                            
         MVC   44(4,R1),AMQMAIL                                                 
*                                                                               
*                                                                               
* READ THE Q MANAGER (MQQMGRNAME=) AND Q NAMES (MQQUEUENAME=).                  
*                                                                               
RC20     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BE    RCX                                                              
*                                                                               
         CLC   =C'MQQMGRNAME=',CARD                                             
         BNE   *+14                                                             
         MVC   QMGRNAME,CARD+11                                                 
         B     RC20                                                             
*                                                                               
         CLC   =C'MQQUEUENAME=',CARD                                            
         BNE   *+14                                                             
         MVC   OBJDESC_OBJECTNAME,CARD+12                                       
         B     RC20                                                             
*                                                                               
         CLC   =C'CORRELID=',CARD                                               
         BNE   *+14                                                             
         MVC   QCORID,CARD+9                                                    
         B     RC20                                                             
*                                                                               
         CLC   =C'MSGID=',CARD                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   QMSGID,CARD+6                                                    
         B     RC20                                                             
*                                                                               
RCX      MVC   MSGDESC_CORRELID,QCORID                                          
         MVC   MSGDESC_MSGID,QMSGID                                             
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
CALLMQ   NMOD1 0,CALLMQ                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* UPON ENTRY,  R2 POINTS TO PARAMETER STRUCTURE                                 
* UPON RETURN, COMPCODE CONTAINS THE MQ COMPLETION CODE                         
*              REASON CONTAINS THE MQ REASON CODE                               
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         L     RF,0(R2)            RF = A(MQ ROUTINE)                           
         LA    R3,24(R2)           R3 = A(PARAMETER LIST)                       
         CALL  (15),MF=(E,(R3))                                                 
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         MVI   P,C'+'              '+' MEANS IT'S AN MQ CALL                    
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODES          
         MVC   P+30(8),=C'COMP. OK'                                             
         CLC   COMPCODE,=A(MQCC_OK)                                             
         BE    CALLMQ30                                                         
         MVC   P+30(8),=C'WARNING!'                                             
         CLC   COMPCODE,=A(MQCC_WARNING)                                        
         BE    CALLMQ10                                                         
         MVC   P+30(8),=C'*FAILED!'                                             
         CLC   COMPCODE,=A(MQCC_FAILED)                                         
         BE    CALLMQ10                                                         
         DC    H'0'                UNKNOWN COMPLETION CODE                      
*                                                                               
CALLMQ10 MVC   P+110(9),=C'*** ERROR'                                           
         MVC   P+39(7),=C'REASON='                                              
         EDIT  REASON,(5,P+47),ZERO=NOBLANK                                     
         MVI   P+52,C':'                                                        
*                                                                               
         L     RF,=A(MQ_REASON_CODE_TABLE)                                      
CALLMQ20 CLI   0(RF),X'FF'         END OF TABLE?                                
         BNE   *+14                NO                                           
         MVC   P+54(22),=C'*** UNKNOWN REASON ***'                              
         B     CALLMQ30            REASON CODE NOT IN TABLE                     
*                                                                               
         CLC   REASON,0(RF)        FIND MATCH ON REASON CODE                    
         BE    *+12                GOT IT                                       
         LA    RF,28(RF)           BUMP TO NEXT TABLE ENTRY                     
         B     CALLMQ20                                                         
*                                                                               
         MVC   P+54(24),4(RF)                                                   
*                                                                               
CALLMQ30 PRNT                                                                   
*                                                                               
         CLC   COMPCODE,=A(MQCC_OK)                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         CMQA LIST=YES,EQUONLY=NO                                               
         SPACE 2                                                                
FILEIN   DCB   DDNAME=FILEIN,MACRF=GM,DSORG=PS,EODAD=GOODBYE                    
         EJECT                                                                  
* MQSERIES CALL PARAMETERS                                                      
*                                                                               
QMGRNAME     DC      CL48' '                                                    
QCORID       DS      XL(L'MSGDESC_CORRELID)                                     
QMSGID       DS      XL(L'MSGDESC_MSGID)                                        
*                                                                               
HCONN        DS      F                     CONNECTION HANDLE                    
HOBJ         DS      F                     OBJECT HANDLE                        
COMPCODE     DS      F                     COMPLETION CODE                      
REASON       DS      F                     QUALIFIES COMPLETION CODE            
BUFFERLENGTH DS      F                     LENGTH OF DATA IN MQBUFFER           
OBJDESC      CMQODA  DSECT=NO,LIST=YES     OBJECT DESCRIPTOR                    
MSGDESC      CMQMDA  DSECT=NO,LIST=YES     MESSAGE DESCRIPTOR                   
PUTMSGOPTS   CMQPMOA DSECT=NO,LIST=YES     PUT MESSAGE OPTIONS                  
*                                                                               
         EJECT                                                                  
DMWORK   DS    12D                                                              
DMCB     DS    10F                                                              
DUB      DS    D                                                                
DUB2     DS    D                                                                
PRNTDUB  DS    D                                                                
RECORD   DS    CL256                                                            
         DS    CL100                                                            
CARD     DS    CL80                                                             
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
PRNTTIME DS    CL9                                                              
WORK     DS    CL256                                                            
ELCODE   DS    X                                                                
TRACEFLG DC    C'Y'                'Y' TO PRINT DETAILED TRACE                  
*                                                                               
AEOMBUFF DS    A                   A(END OF MAIL BUFFER)                        
AMQMAIL  DS    A                   A(MAIL BUFFER)                               
*                                                                               
         EJECT                                                                  
* PARAMETER LISTS FOR MQSERIES CALLS                                            
*  F    A(ROUTINE)                                                              
*  CL16 EBCDIC ROUTINE NAME                                                     
*  XL1  FLAGS                                                                   
*  XL3  SPARE                                                                   
*  PARAMETERS (STANDARD IBM FORMAT)                                             
*                                                                               
CSQBCONN_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_CONNECT'                                
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(QMGRNAME)                             
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(COMPCODE)                             
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBDISC_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_DISCONNECT'                             
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(COMPCODE)                             
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBPUT1_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_PUT1'                                   
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(OBJDESC)                              
                          DC    X'00',AL3(MSGDESC)                              
                          DC    X'00',AL3(PUTMSGOPTS)                           
                          DC    X'00',AL3(BUFFERLENGTH)                         
                          DS    AL4                                             
                          DC    X'00',AL3(COMPCODE)                             
                          DC    X'80',AL3(REASON)                               
         SPACE 3                                                                
ENTRYPTS DS    0F                                                               
         DC    Y((ENTRYLSQ-ENTRYSTQ)/60)   NUMBER OF TABLE ENTRIES              
         DC    H'60'                       MUST REMAIN AS 60                    
ENTRYSTQ EQU   *                                                                
CSQBCONN DC    CL8'CSQBCONN'                                                    
         DC    XL52'00'                                                         
CSQBDISC DC    CL8'CSQBDISC'                                                    
         DC    XL52'00'                                                         
CSQBPUT1 DC    CL8'CSQBPUT1'                                                    
         DC    XL52'00'                                                         
ENTRYLSQ EQU   *                                                                
         EJECT                                                                  
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
         EJECT                                                                  
       ++INCLUDE DDMQREASON                                                     
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051DDMQPUT1  08/17/04'                                      
         END                                                                    
