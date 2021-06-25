*          DATA SET YYUNINVC   AT LEVEL 004 AS OF 04/02/01                      
*PHASE YYUNINVC                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE DMDMGRL                                                                
         TITLE 'MVS INVOICE DATASET TO LOTUS NOTES VIA MQSERIES'                
YYUNINVC CSECT                                                                  
         PRINT NOGEN                                                            
START    NBASE 0,*INVOICE,=A(R13CHAIN),R9                                       
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         BRAS  RE,INITIAL          INITIALIZE                                   
*                                                                               
         LA    R2,CSQBCONN_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     CONNECT TO MQ QUEUE MANAGER                  
*                                                                               
         MVC   OBJDESC_OBJECTTYPE,=A(MQOT_Q)  OBJECT IS A QUEUE                 
         MVC   OBJDESC_OBJECTNAME,QNAME  INPUT QUEUE NAME                       
*                                                                               
         LA    RF,MQPMO_SYNCPOINT                                               
         ICM   RE,15,=AL4(MQPMO_FAIL_IF_QUIESCING)                              
         AR    RF,RE                                                            
         ST    RF,PUTMSGOPTS_OPTIONS                                            
         MVC   MSGDESC_PERSISTENCE,=A(MQPER_PERSISTENT)                         
*                                                                               
         OPEN  FILEIN                                                           
         LTR   RF,RF                                                            
         BZ    *+8                                                              
         BRAS  R7,DIE                                                           
*                                                                               
NEXTBUF  L     R8,=A(BUFFER)                                                    
*                                                                               
         XC    BUFFERLENGTH,BUFFERLENGTH                                        
         MVC   0(23,R8),=C'NOTES^^YYUNN^^INVOICES^'                             
         AHI   R8,23                                                            
*                                                                               
GETREC   GET   FILEIN,RECORD                                                    
*                                                                               
         MVC   0(256,R8),RECORD                                                 
         MVC   256(L'RECORD-256,R8),RECORD+256                                  
         AHI   R8,L'RECORD-1      POINT TO LAST CHAR                            
NEXTCHAR CLI   0(R8),C' '         FIND LAST SIGNIFICANT CHARACTER               
         BE    *+12                                                             
         CLI   0(R8),X'00'                                                      
         BNE   *+8                                                              
         BCT   R8,NEXTCHAR                                                      
*                                                                               
         MVI   1(R8),X'25'        LINE BREAK                                    
         LA    R8,2(R8)           R8 POINTS JUST PAST END OF DATA               
*                                                                               
         LR    RF,R8                                                            
         S     RF,=A(BUFFER)                                                    
         ST    RF,BUFFERLENGTH                                                  
*                                                                               
         CLC   =C'34',RECORD      END OF INVOICE REC                            
         BNE   GETREC             NO, KEEP GOING                                
*                                 YES, CHECK THE LENGTH NOW                     
         CLC   BUFFERLENGTH,=F'2000000'  ALMOST 2 MEGA BYTES?                   
         BL    GETREC              LESS THAN 2M, READ MORE                      
*                                                                               
         CLC   BUFFERLENGTH,=F'4194304'   EXCESS 4M LIMIT?                      
         BL    *+8                                                              
         BRAS  R7,DIE              YES, CAN'T SEND                              
*                                                                               
         LA    R2,CSQBPUT1_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     PUT THE MESSAGE TO THE MQ QUEUE              
         B     NEXTBUF                                                          
*                                                                               
GOODBYE  CLOSE FILEIN                                                           
         LTR   RF,RF                                                            
         BZ    *+8                                                              
         BRAS  R7,DIE                                                           
*                                                                               
         OC    BUFFERLENGTH,BUFFERLENGTH  NO RECORD TO SEND                     
         BZ    DISC                                                             
*                                                                               
         CLC   BUFFERLENGTH,=F'4194304'   EXCESS 4M LIMIT?                      
         BL    *+8                                                              
         BRAS  R7,DIE              YES, CAN'T SEND                              
*                                                                               
         LA    R2,CSQBPUT1_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     PUT THE LAST MESSAGE TO THE MQ QUEUE         
*                                                                               
DISC     LA    R2,CSQBDISC_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     DISCONNECT FROM MQ QUEUE MANAGER             
*                                                                               
         XBASE                                                                  
         LTORG                                                                  
*                                                                               
DIE      DS    0H                                                               
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'OPMSG',=C'AUTONOTE*YYUN,BGRI:INVOICE+        
                TO LOTUS FAILED, PLEASE CHECK.'                                 
         SHI   R7,4                                                             
         S     R7,=A(START)        R7 = OFFSET WHERE IT DIED                    
         DC    H'0'                                                             
         EJECT                                                                  
INITIAL  NTR1                                                                   
*                                                                               
         BLDL  0,ENTRYPTS            BUILD LIST OF APPC/MVS ENTRY PTS           
         LTR   RF,RF                                                            
         BZ    *+8                                                              
         BRAS  R7,DIE                BAD RETURN FROM BLDL MACRO                 
*                                                                               
         LOAD  DE=CSQBCONN                                                      
         ST    R0,CSQBCONN_STRUCTURE                                            
         LOAD  DE=CSQBPUT1                                                      
         ST    R0,CSQBPUT1_STRUCTURE                                            
         LOAD  DE=CSQBDISC                                                      
         ST    R0,CSQBDISC_STRUCTURE                                            
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
CALLMQ   NTR1                                                                   
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
         BRAS  R7,DIE              UNKNOWN COMPLETION CODE                      
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
CALLMQ30 GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLC   COMPCODE,=A(MQCC_OK)                                             
         BE    *+8                                                              
         BRAS  R7,DIE                                                           
*                                                                               
         XIT1                                                                   
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         CMQA LIST=YES,EQUONLY=NO                                               
         SPACE 2                                                                
FILEIN   DCB   DDNAME=FILE,MACRF=GM,DSORG=PS,EODAD=GOODBYE                      
         EJECT                                                                  
* MQSERIES CALL PARAMETERS                                                      
*                                                                               
QMGRNAME     DC      CL48'MQ1P'                                                 
QNAME        DC      CL48'INVOICE.REMOTEQ'                                      
*                                                                               
HCONN        DS      F         MQ QMGR CONNECTION HANDLE                        
HOBJ         DS      F         OBJECT HANDLE                                    
COMPCODE     DS      F         COMPLETION CODE                                  
REASON       DS      F         QUALIFIES COMPLETION CODE                        
BUFFERLENGTH DS      F         LENGTH OF MQBUFFER AREA                          
OBJDESC      CMQODA  LIST=YES  OBJECT DESCRIPTOR                                
MSGDESC      CMQMDA  LIST=YES  MESSAGE DESCRIPTOR                               
         ORG   MSGDESC_FORMAT                                                   
         DC  CL8'MQSTR   '     MQFMT_STRING  FOR DATA FORMAT                    
         ORG                                                                    
PUTMSGOPTS   CMQPMOA LIST=YES  PUT MESSAGE OPTIONS                              
         EJECT                                                                  
DMWORK   DS    12D                                                              
DMCB     DS    10F                                                              
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
RECORD   DS    CL360                                                            
         DS    CL100                                                            
WORK     DS    CL256                                                            
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
CSQBPUT1_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_PUT1'                                   
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(OBJDESC)                              
                          DC    X'00',AL3(MSGDESC)                              
                          DC    X'00',AL3(PUTMSGOPTS)                           
                          DC    X'00',AL3(BUFFERLENGTH)                         
                          DC    X'00',AL3(BUFFER)                               
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
         DS    0D                                                               
         DC    C'MQBUFFER'                                                      
BUFFER   DS    4194304C            MQ BUFFER                                    
         DC    C'MQBUFFER'                                                      
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
**PAN#1  DC    CL21'004YYUNINVC  04/02/01'                                      
         END                                                                    
