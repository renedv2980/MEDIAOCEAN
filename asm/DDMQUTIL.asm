*          DATA SET DDMQUTIL   AT LEVEL 002 AS OF 05/20/20                      
*PHASE DDMQUTLA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE VALCARD                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE DDWTO                                                                  
*INCLUDE DMDMGRL                                                                
         TITLE 'MQ UTILITY TO GET/PUT FILES/RECORDS TO MQ'                      
         PRINT NOGEN                                                            
DDMQPAN  CSECT                                                                  
         COPY  IEABRC                                                           
         NBASE WORKX-WORKD,**MQUTIL,=A(WORKAREA),CLEAR=YES                      
         USING WORKD,RC                                                         
         J     START               JUMP OVER DATA AREA                          
                                                                                
$$DATA   LOCTR ,                   DATA LOCATION HERE AFTER RB                  
***********************************************************************         
* MAIN CODE AREA                                                      *         
***********************************************************************         
$$CODE   LOCTR ,                   CODE AFTER DATA                              
                                                                                
START    ST    RD,SAVERD           SAVE RD FOR EXIT                             
*                                                                               
         BRAS  RE,INIT             READ CARDS ECT                               
*                                                                               
MAIN010  CLI   TYPE,C'D'           WHOLE FILE GET/PUT AS DATA                   
         JE    MAIN020                                                          
         CLI   TYPE,C'V'           VB RECORDS                                   
         JE    MAIN040                                                          
         CLI   TYPE,C'F'           FB RECORDS                                   
         JE    MAIN060                                                          
         CLI   TYPE,C'X'           FILE XFER - WAS DDMQXFR                      
         JE    MAIN080                                                          
*                                                                               
         MVC   P(16),=C'INVALID TYPE  //'                                       
         J     ERRX                                                             
                                                                                
MAIN020  CLI   MODE,C'P'           MODE=PUT,TYPE=DATA                           
         JNE   MAIN030                                                          
         BRAS  RE,MAINPUT          PUT TO MQ AS BLOCKS                          
         J     XBASE                                                            
*                                                                               
MAIN030  CLI   MODE,C'G'           MODE=GET,TYPE=DATA                           
         JNE   XBASE                                                            
         BRAS  RE,MAINGET          GET FROM MQ QUEUE AS BLOCKS                  
         J     XBASE                                                            
*                                                                               
MAIN040  CLI   MODE,C'P'           MODE=PUT,TYPE=VB                             
         JNE   MAIN050                                                          
         BRAS  RE,MAINPUTV         PUT VB RECORDS TO MQ                         
         J     XBASE                                                            
*                                                                               
MAIN050  CLI   MODE,C'G'           MODE=GET,TYPE=VB                             
         JNE   XBASE                                                            
         BRAS  RE,MAINGETV         GET VB RECORDS FROM MQ                       
         J     XBASE                                                            
*                                                                               
MAIN060  CLI   MODE,C'P'           MODE=PUT,TYPE=FB                             
         JNE   MAIN070                                                          
         BRAS  RE,MAINPUTF         PUT FB RECORDS TO MQ                         
         J     XBASE                                                            
*                                                                               
MAIN070  CLI   MODE,C'G'           MODE=GET,TYPE=FB                             
         JNE   XBASE                                                            
         BRAS  RE,MAINGETF         GET FB RECORDS FROM MQ                       
         J     XBASE                                                            
*                                                                               
MAIN080  CLI   MODE,C'P'           MODE=PUT,TYPE=XFER                           
         JNE   MAIN090                                                          
         BRAS  RE,MAINPUTX         PUT FILE TO MQ QUEUE AS RECORDS              
         J     XBASE                                                            
*                                                                               
MAIN090  CLI   MODE,C'G'           MODE=GET,TYPE=XFER                           
         JNE   XBASE                                                            
         BRAS  RE,MAINSYNC         SYNC TO NEXT FILE HEADER                     
         JNE   XBASE                                                            
         BRAS  RE,MAINGETX         GET FILE FROM MQ QUEUE AS RECORDS            
         J     XBASE                                                            
*                                                                               
ERRX     GOTO1 =V(DDWTO),DMCB,P,(X'80',0)                                       
         J     XBASE16                                                          
*                                                                               
EXITOK   CR    RB,RB               EXIT CC EQU                                  
         J     EXIT                                                             
*                                                                               
EXITL    XR    R0,R0               EXIT CC LOW                                  
         CR    R0,RB                                                            
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
XBASE16  MVI   RETCODE,16                                                       
*                                                                               
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE RC=RETCODE,RL=1                                                  
         EJECT                                                                  
************************************************************                    
*        INIT MQ / READ CARDS /OPEN DATASETS               *                    
************************************************************                    
         SPACE 1                                                                
INIT     NTR1                                                                   
*                                                                               
         BLDL  0,ENTRYPTS            BUILD LIST OF APPC/MVS ENTRY PTS           
         LTR   RF,RF                                                            
         JZ    *+6                                                              
         DC    H'0'                  BAD RETURN FROM BLDL MACRO                 
*                                                                               
         LOAD  DE=CSQBCONN                                                      
         ST    R0,AMQBCONN                                                      
         LOAD  DE=CSQBOPEN                                                      
         ST    R0,AMQBOPEN                                                      
         LOAD  DE=CSQBPUT                                                       
         ST    R0,AMQBPUTN                                                      
         LOAD  DE=CSQBGET                                                       
         ST    R0,AMQBGETN                                                      
         LOAD  DE=CSQBCOMM                                                      
         ST    R0,AMQBCOMM                                                      
         LOAD  DE=CSQBCLOS                                                      
         ST    R0,AMQBCLOS                                                      
         LOAD  DE=CSQBDISC                                                      
         ST    R0,AMQBDISC                                                      
*                                                                               
         LA    R3,CARD             DO CARD VALIDATION                           
*                                                                               
INIT02   GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         CLC   =C'/*',0(R3)                                                     
         JE    INIT04                                                           
*                                                                               
         L     RF,=V(DDSIO)        SET UP DDSIO FIRST                           
         MVC   0(8,RF),DDSIO                                                    
*                                                                               
         XC    VCCB,VCCB           MUST BE CLEARED BEFORE                       
NEXTVAL  LA    RF,CARD                                                          
         ST    RF,VCPCARD          SET P1 CARD                                  
         LARL  RF,CARDTAB                                                       
         ST    RF,VCPTABLE         SET P2 TABLE                                 
         GOTO1 =V(VALCARD),VCCB                                                 
         JNE   XBASE16                                                          
         L     R2,VCPVALUE         A VALUE TO EXECUTE ON                        
         L     R0,VCPLEN           LEN OF VALUE                                 
         LT    RF,VCPROUT          A ROUTINE TO EXECUTE                         
         JZ    INIT02                                                           
         BASR  RE,RF               DO VALIDATION ROUTINE                        
         OC    VCPNEXT,VCPNEXT     ANY POINTER TO NEXT KEYWORD                  
         JNZ   NEXTVAL             LOOP BACK TO DO IT                           
         J     INIT02              LOOP FOR NEXT CARD                           
*                                                                               
INIT04   EQU   *                                                                
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
************************************************************                    
* MQ SEQUENCES OPEN CLOSE AND COMMIT COUNT CHECK           *                    
************************************************************                    
OPENMQ   NTR1                                                                   
         BRAS  RE,MQCONNEC         CONNECT TO MQ QUEUE MANAGER                  
         MVC   OBJDESC_OBJECTTYPE,=A(MQOT_Q)  OBJECT IS A QUEUE                 
         MVC   OBJDESC_OBJECTNAME,QUEUE       INPUT QUEUE NAME                  
*                                                                               
         CLI   MODE,C'P'           MODE=PUT                                     
         JNE   *+8                                                              
         LA    RF,MQOO_OUTPUT      SET OPTIONS OUTPUT                           
*                                                                               
         CLI   MODE,C'G'           MODE=GET                                     
         JNE   *+8                                                              
         LA    RF,MQOO_INPUT_AS_Q_DEF  SET OPTIONS INPUT                        
*                                                                               
         ST    RF,OPENOPT                                                       
         BRAS  RE,MQOPEN           OPEN QUEUE                                   
*                                                                               
         MVC   MSGDESC_CORRELID,QCORID        SET QCORID                        
         MVC   MSGDESC_MSGID,QMSGID           SET QMSGID                        
*                                                                               
OPENM010 CLI   MODE,C'G'           MODE=GET                                     
         JNE   OPENM020                                                         
         LA    RF,MQPMO_SYNCPOINT             SET OPTIONS                       
         ICM   RE,15,=AL4(MQPMO_FAIL_IF_QUIESCING)                              
         AR    RF,RE                                                            
         ST    RF,GETOPTS_OPTIONS                                               
         J     EXIT                                                             
*                                                                               
OPENM020 CLI   MODE,C'P'           MODE=PUT                                     
         JNE   EXIT                                                             
         LA    RF,MQPMO_SYNCPOINT             SET OPTIONS                       
         ICM   RE,15,=AL4(MQPMO_FAIL_IF_QUIESCING)                              
         AR    RF,RE                                                            
         ST    RF,PUTOPTS_OPTIONS                                               
         J     EXIT                                                             
*                                                                               
EXITMQ   BRAS  RE,MQCOMMIT         COMMIT                                       
         LA    RF,MQCO_NONE        SET CLOSE OPTIONS                            
         ST    RF,CLOSEOPT                                                      
         BRAS  RE,MQCLOSE          CLOSE QUEUE                                  
         BRAS  RE,MQDISCON         DISCONNECT FROM MQ QUEUE MANAGER             
         J     EXIT                                                             
*                                                                               
COUNTMQ  L     R1,COUNTER          COUNTER TO MUST COMMIT POINT                 
         AHI   R1,1                                                             
         ST    R1,COUNTER                                                       
         CLC   COUNTER,COMCOUNT    MUST COMMIT AT COMCOUNT                      
         BLR   RE                                                               
         ST    RE,SAVERE                                                        
         BRAS  RE,MQCOMMIT         COMMIT                                       
         LA    R1,1                                                             
         ST    R1,COUNTER          SET COUNT TO 1 AGAIN                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
************************************************************                    
*        MAIN PROGRAM TO PUT FILE TO MQ AS BLOCKS          *                    
************************************************************                    
         SPACE 1                                                                
MAINPUT  NTR1                                                                   
         LA    R8,TAPEIN           R8 = DCB                                     
         USING IHADCB,R8                                                        
*                                                                               
         BRAS  RE,OPENMQ           CONNECT TO MQ QUEUE MANAGER                  
*                                                                               
         OPEN  (TAPEIN,INPUT)      OPEN INPUT FILE                              
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
GETREC   GET   TAPEIN              GET RECORD (R2)                              
         LR    R2,R1                                                            
         LH    R1,DCBLRECL         GET LENGTH OF RECORD                         
         ST    R1,BUFFLEN          SET AS BUFFLEN                               
         ST    R1,DATALEN          SAVE HERE FOR WTO MESSAGE                    
*                                                                               
         LA    RE,BUFFER           COPY RECORD TO BUFFER                        
         L     RF,=A(L'BUFFER)                                                  
         LR    R0,R2                                                            
         MVCL  RE,R0                                                            
*                                                                               
         BRAS  RE,MQPUT            PUT THE MESSAGE TO THE MQ QUEUE              
         J     GETREC              LOOP BACK FOR MORE                           
*                                                                               
GOODBYE  CLOSE TAPEIN              EOF ENTRY POINT                              
         J     EXITMQ                                                           
*                                                                               
         EJECT                                                                  
************************************************************                    
*        MAIN PROGRAM TO GET FILE FROM MQ AS BLOCKS        *                    
************************************************************                    
         SPACE 1                                                                
MAINGET  NTR1                                                                   
         LA    R8,TAPEOUT          R8=DCB                                       
         USING IHADCB,R8                                                        
*                                                                               
         BRAS  RE,OPENMQ           CONNECT TO MQ QUEUE MANAGER                  
*                                                                               
         OPEN  (TAPEOUT,OUTPUT)    OPEN OUTPUT FILE                             
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
PUTREC   MVC   BUFFLEN,=A(L'BUFFER)                                             
*                                                                               
         BRAS  RE,MQGET            GET THE MESSAGE FROM MQ QUEUE                
         JNE   ENDOFMSG                                                         
*                                                                               
         L     R1,DATALEN          USE DATALEN FOR DCB WRITE                    
         STH   R1,DCBLRECL                                                      
         PUT   TAPEOUT,BUFFER                                                   
         J     PUTREC              LOOP BACK FOR MORE                           
*                                                                               
ENDOFMSG CLOSE TAPEOUT                                                          
         J     EXITMQ                                                           
         EJECT                                                                  
************************************************************                    
*        MAIN PROGRAM TO PUT VB RECORDS TO MQ              *                    
************************************************************                    
         SPACE 1                                                                
MAINPUTV NTR1                                                                   
*                                                                               
         BRAS  RE,OPENMQ           CONNECT TO MQ QUEUE MANAGER                  
*                                                                               
         XC    COUNTER,COUNTER     CLEAR COUNTER                                
*                                                                               
         OPEN  (TAPEINV,INPUT)     OPEN INPUT FILE                              
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
GETRECV  CLI   SPLIT,C'Y'            KEEP MESSAGE ID TO SPLIT                   
         JE    *+10                                                             
         MVC   MSGDESC_MSGID,QMSGID  OR RESET MESSAGE ID                        
*                                                                               
         GET   TAPEINV             GET RECORD (R2)                              
*                                                                               
         LR    R2,R1                                                            
         ICM   R1,15,0(R2)         GET RECORD LEN                               
         SRL   R1,16               SHIFT AND -4                                 
         SHI   R1,4                                                             
         ST    R1,BUFFLEN          MOVE TO BUFFLEN                              
         LA    RE,BUFFER                                                        
         LR    RF,R1                                                            
         LA    R0,4(R2)                                                         
         MVCL  RE,R0               COPY RECORD TO BUFFER                        
*                                                                               
         BRAS  RE,COUNTMQ          COUNTER TO MUST COMMIT POINT                 
*                                                                               
         BRAS  RE,MQPUT            PUT THE MESSAGE TO THE MQ QUEUE              
         J     GETRECV                                                          
*                                                                               
GOODBYEV CLI   TYPE,C'X'           ARE WE ON XFER OR VB                         
         JE    GOODBYEX            XFER GOTO GOODBYEX                           
*                                                                               
         CLOSE TAPEINV                                                          
         J     EXITMQ                                                           
         EJECT                                                                  
************************************************************                    
*        MAIN PROGRAM TO GET VB RECORDS FROM MQ            *                    
************************************************************                    
         SPACE 1                                                                
MAINGETV NTR1                                                                   
*                                                                               
         BRAS  RE,OPENMQ           CONNECT TO MQ QUEUE MANAGER                  
*                                                                               
         XC    COUNTER,COUNTER                                                  
*                                                                               
         OPEN  (TAPEOUTV,OUTPUT)   OPEN OUTPUT FILE                             
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
PUTRECV  BRAS  RE,COUNTMQ          COUNTER TO MUST COMMIT POINT                 
*                                                                               
         MVC   BUFFLEN,=A(L'BUFFER)                                             
*                                                                               
         CLI   SPLIT,C'Y'            KEEP MESSAGE ID TO SPLIT                   
         JE    *+10                                                             
         MVC   MSGDESC_MSGID,QMSGID  OR RESET MESSAGE ID                        
*                                                                               
         BRAS  RE,MQGET            GET THE MESSAGE FROM MQ QUEUE                
         JNE   ENDOFV                                                           
*                                                                               
GETV052  XC    IOL,IOL             SET RECORD LEN                               
         L     R1,DATALEN                                                       
         LA    R1,4(R1)                                                         
         SLL   R1,16                                                            
         ST    R1,IOL                                                           
         PUT   TAPEOUTV,IOL        PUT TO OUTPUT FILE                           
         J     PUTRECV                                                          
*                                                                               
ENDOFV   CLOSE TAPEOUT                                                          
         J     EXITMQ                                                           
         EJECT                                                                  
************************************************************                    
*        MAIN PROGRAM TO PUT FB RECORDS TO MQ              *                    
************************************************************                    
         SPACE 1                                                                
MAINPUTF NTR1                                                                   
*                                                                               
         BRAS  RE,OPENMQ           CONNECT TO MQ QUEUE MANAGER                  
*                                                                               
         XC    COUNTER,COUNTER     CLEAR COUNTER                                
*                                                                               
         OPEN  (TAPEINF,INPUT)     OPEN INPUT FILE                              
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
GETRECF  GET   TAPEINF,RECORD      GET TO RECORD                                
*                                                                               
         CLI   CONCAT,C'Y'         CONCAT OPTION                                
         JNE   GETRECF1                                                         
*                                                                               
         LA    R6,BUFFER           CONCAT=Y SET UP BUFFER                       
         L     R1,WIDTH                                                         
         AHI   R1,-1                                                            
         EXRL  R1,*+10                                                          
         J     *+10                                                             
         MVC   BUFFER(0),RECORD    MOVE 1ST TO BUFFER                           
*                                                                               
GETRECF0 A     R6,WIDTH            LOOP THROUGH                                 
         GET   TAPEINF,(R6)        GET STRAIGHT TO BUFFER                       
         J     GETRECF0            UNTIL EOF SENDS US TO GOODBYEF               
*                                                                               
* CONCAT=N SO BACK UP TO LAST SIGNIFICANT CHR AND COPY TO BUFFER                
*                                                                               
GETRECF1 LA    RF,RECORD+L'RECORD-1                                             
*                                                                               
GETRECF2 CLI   0(RF),C' '          FIND LAST SIGNIFICANT CHARACTER              
         JH    GETRECF3                                                         
         BCTR  RF,0                                                             
         LA    R1,RECORD                                                        
         CR    RF,R1                                                            
         JH    GETRECF2                                                         
         DC    H'0'                                                             
*                                                                               
GETRECF3 LA    RF,1(RF)            RF POINTS JUST PAST END OF DATA              
         LA    R1,RECORD                                                        
         SR    RF,R1                                                            
         ST    RF,BUFFLEN          LENGTH OF DATA                               
*                                                                               
         XC    BUFFER+000(255),BUFFER+000                                       
         XC    BUFFER+255(255),BUFFER+255                                       
         BCTR  RF,0                                                             
         EXRL  RF,*+10                                                          
         J     *+10                                                             
         MVC   BUFFER(0),RECORD    MOVE DATA TO MQ BUFFER AREA                  
         J     GETRECF5                                                         
*                                                                               
* CONCAT=Y SO PUT WHOLE BUFFER                                                  
*                                                                               
GETRECF4 LA    RE,BUFFER           HERE FROM GOODBYEF IF CONCAT                 
         LR    RF,R6                                                            
         SR    RF,RE                                                            
         ST    RF,BUFFLEN          LENGTH OF DATA                               
*                                                                               
* PUT RECORD TO MQ                                                              
*                                                                               
GETRECF5 BRAS  RE,COUNTMQ          COUNTER TO MUST COMMIT POINT                 
*                                                                               
         CLI   SPLIT,C'Y'            KEEP MESSAGE ID TO SPLIT                   
         JE    *+10                                                             
         MVC   MSGDESC_MSGID,QMSGID  OR RESET MESSAGE ID                        
*                                                                               
         BRAS  RE,MQPUT            PUT THE MESSAGE TO THE MQ QUEUE              
*                                                                               
PUTF050  CLI   CONCAT,C'Y'         CONCAT THEN DONE                             
         JE    GOODBYF3                                                         
         J     GETRECF             LOOP BACK FOR NEXT RECORD                    
*                                                                               
* EOF BRANCHES                                                                  
*                                                                               
GOODBYEF DS    0H                                                               
         CLI   CONCAT,C'Y'         CONCAT                                       
         JE    GETRECF4                                                         
*                                                                               
GOODBYF3 CLOSE TAPEINF                                                          
         J     EXITMQ                                                           
         EJECT                                                                  
************************************************************                    
*        MAIN PROGRAM TO GET FB RECORDS FROM MQ            *                    
************************************************************                    
         SPACE 1                                                                
MAINGETF NTR1                                                                   
*                                                                               
         BRAS  RE,OPENMQ           CONNECT TO MQ QUEUE MANAGER                  
*                                                                               
         XC    COUNTER,COUNTER                                                  
*                                                                               
         OPEN  (TAPEOUTF,OUTPUT)   OPEN OUTPUT FILE                             
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
PUTRECF  MVC   BUFFLEN,=A(L'BUFFER)                                             
*                                                                               
         BRAS  RE,COUNTMQ          COUNTER TO MUST COMMIT POINT                 
*                                                                               
         CLI   SPLIT,C'Y'          KEEP MESSAGE ID TO SPLIT                     
         JE    *+10                                                             
         MVC   MSGDESC_MSGID,QMSGID  OR RESET MESSAGE ID                        
*                                                                               
         BRAS  RE,MQGET            GET THE MESSAGE FROM MQ QUEUE                
         JNE   ENDOFF                                                           
*                                                                               
GETF052  PUT   TAPEOUTF,BUFFER     PUT TO OUTPUT FILE                           
         J     PUTRECF                                                          
*                                                                               
ENDOFF   CLOSE TAPEOUT                                                          
         J     EXITMQ                                                           
         EJECT                                                                  
************************************************************                    
*        MAIN PROGRAM TO PUT TO MQ                         *                    
************************************************************                    
         SPACE 1                                                                
MAINPUTX NTR1                                                                   
*                                                                               
         BRAS  RE,OPENMQ           CONNECT TO MQ QUEUE MANAGER                  
*                                                                               
         XC    COUNTER,COUNTER     CLEAR COUNTER                                
         XC    SEQN,SEQN           CLEAR SEQ                                    
*                                                                               
         OPEN  (TAPEINV,INPUT)     OPEN INPUT FILE                              
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         MVC   BUFFER(32),INITREC  PUT HEADER RECORD                            
         MVC   BUFFLEN,=F'32'                                                   
         J     PUTX050                                                          
*                                                                               
GETRECX  GET   TAPEINV                                                          
*                                                                               
         LR    R2,R1                                                            
         ICM   R1,15,0(R2)                                                      
         SRL   R1,16                                                            
         ST    R1,BUFFLEN                                                       
         SHI   R1,4                                                             
         LA    RE,BUFFER+4                                                      
         LR    RF,R1                                                            
         LA    R0,4(R2)                                                         
         MVCL  RE,R0                                                            
         MVC   BUFFER(4),SEQN      PUT SEQ IN BUFFER+0(4)                       
*                                                                               
         L     R1,SEQN             SEQUENCE COUNTER                             
         AHI   R1,1                                                             
         ST    R1,SEQN                                                          
*                                                                               
PUTX050  BRAS  RE,COUNTMQ          COUNTER TO MUST COMMIT POINT                 
         BRAS  RE,MQPUT            PUT THE MESSAGE TO THE MQ QUEUE              
         J     GETRECX                                                          
*                                                                               
GOODBYEX DS    0H                                                               
         MVC   BUFFER(32),EOFREC   PUT FOOTER RECORD                            
         MVC   BUFFLEN,=F'32'                                                   
         BRAS  RE,MQPUT            PUT THE MESSAGE TO THE MQ QUEUE              
*                                                                               
         CLOSE TAPEINV                                                          
         J     EXITMQ                                                           
         EJECT                                                                  
************************************************************                    
*        MAIN PROGRAM TO SYNCHRONISE TO HEADER             *                    
************************************************************                    
         SPACE 1                                                                
MAINSYNC NTR1                                                                   
*                                                                               
         BRAS  RE,OPENMQ           CONNECT TO MQ QUEUE MANAGER                  
*                                                                               
         MVI   FLAG,C'X'           FLAG UNKNOWN                                 
*                                                                               
SYN050   MVC   BUFFLEN,=A(L'BUFFER)                                             
*                                                                               
         BRAS  RE,COUNTMQ          COUNTER TO MUST COMMIT POINT                 
         BRAS  RE,MQGET            GET THE MESSAGE FROM MQ QUEUE                
         JNE   ENDOFSYN                                                         
*                                                                               
SYN051   CLC   BUFFER(32),INITREC  KEEP GOING TILL WE GET INIT REC              
         JNE   SYN050                                                           
         MVI   FLAG,C'I'           FLAG INIT FOUND                              
*                                                                               
ENDOFSYN DS    0H                                                               
         BRAS  RE,MQCOMMIT         COMMIT                                       
*                                                                               
         LA    RF,MQCO_NONE                                                     
         ST    RF,CLOSEOPT                                                      
*                                                                               
         BRAS  RE,MQCLOSE          CLOSE QUEUE                                  
         BRAS  RE,MQDISCON         DISCONNECT FROM MQ QUEUE MANAGER             
*                                                                               
         CLI   FLAG,C'I'           INIT FOUND THEN EXIT OK                      
         JE    EXITOK                                                           
         J     EXITL                                                            
         EJECT                                                                  
************************************************************                    
*        MAIN PROGRAM TO GET FROM MQ                       *                    
************************************************************                    
         SPACE 1                                                                
MAINGETX NTR1                                                                   
*                                                                               
         BRAS  RE,OPENMQ           CONNECT TO MQ QUEUE MANAGER                  
*                                                                               
         XC    COUNTER,COUNTER                                                  
         XC    SEQN,SEQN                                                        
*                                                                               
         OPEN  (TAPEOUTV,OUTPUT)   OPEN OUTPUT FILE                             
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
PUTRECX  BRAS  RE,COUNTMQ          COUNTER TO MUST COMMIT POINT                 
*                                                                               
         MVC   BUFFLEN,=A(L'BUFFER)                                             
*                                                                               
GETX050  BRAS  RE,MQGET            GET THE MESSAGE FROM MQ QUEUE                
         JE    GETX055                                                          
*                                                                               
         L     R0,WAITS                                                         
         AHI   R0,1                                                             
         ST    R0,WAITS                                                         
         CLC   WAITS,MAXWAIT                                                    
         JH    ENDOFMSX                                                         
*                                                                               
         MVC   P(20),=C'WAITING FOR DATA  //'                                   
         GOTO1 =V(DDWTO),DMCB,P,(X'80',0)                                       
         L     R1,=A(38400*30)                                                  
         ST    R1,FULL                                                          
         STIMER WAIT,TUINTVL=FULL  WAIT 30 SECS THEN TRY AGAIN                  
         J     GETX050                                                          
*                                                                               
GETX055  XC    WAITS,WAITS         CLEAR MAITS                                  
         CLC   BUFFER(32),EOFREC   EOF FOUND THEN EXIT OK                       
         BE    ENDOFMSG                                                         
*                                                                               
         CLC   SEQN,BUFFER                                                      
         JH    *+2                                                              
*                                                                               
GETX060  MVC   SEQN,BUFFER                                                      
*                                                                               
         XC    BUFFER(4),BUFFER                                                 
         L     R1,DATALEN                                                       
         SLL   R1,16                                                            
         ST    R1,BUFFER                                                        
         PUT   TAPEOUTV,BUFFER                                                  
         J     PUTRECX                                                          
*                                                                               
ENDOFMSX CLOSE TAPEOUTV                                                         
         J     EXITMQ                                                           
         EJECT                                                                  
*************************************************************                   
*        PARAMETER CARDS FOR VALCARD                        *                   
*************************************************************                   
*                                                                               
*        CL8'KEYWORD',AL1(L'KEY,L'OUT),X'FLAGS',AL4(OUTPUT)                     
*                                                                               
*FLAGS   X'8000'                   A(OUTPUT) IS A(ROUTINE)                      
*        X'4000'                   ACCEPT =,/=                                  
*        X'2000'                   ACCEPT <,>,<=,>=                             
*        X'1000'                   HEX VALUE                                    
*        X'0800'                   DEC VALUE                                    
*        X'0400'                   OUTPUT IS A LIST                             
*        X'0200'                   TIME VALUE                                   
*        X'0100'                   DATE VALUE                                   
*        X'0080'                   A(OUTPUT) IS A VALIDATION EXTENSION          
*        X'0040'                   ALLOW PARTIAL KEYWORD                        
*                                                                               
         SPACE 1                                                                
         DS    0F                                                               
CARDTAB  DC    C'DDSIO   ',AL1(5,08),X'0000',A(DDSIO)                           
         DC    C'WIDTH   ',AL1(5,04),X'0800',A(WIDTH)                           
         DC    C'COMMIT  ',AL1(6,04),X'0800',A(COMCOUNT)                        
         DC    C'MAXWAIT ',AL1(7,04),X'0800',A(MAXWAIT)                         
*                                                                               
         DC    C'TYPE    ',AL1(4,08),X'0080',A(TYPEV)                           
         DC    C'MODE    ',AL1(4,08),X'0080',A(MODEV)                           
         DC    C'CONCAT  ',AL1(6,01),X'0080',A(CONCATV)                         
         DC    C'SPLIT   ',AL1(5,01),X'0080',A(SPLITV)                          
         DC    C'QMGR    ',AL1(4,48),X'0080',A(QMGRV)                           
         DC    C'QUEUE   ',AL1(5,48),X'0080',A(QUEUEV)                          
         DC    C'QCORID  ',AL1(6,24),X'0080',A(QCORIDV)                         
         DC    C'QMSGID  ',AL1(6,24),X'0080',A(QMSGIDV)                         
         DC    X'0000'                                                          
*                                                                               
TYPEV    DC    A(TYPE),C'DATA/FB/VB/XFER:'                                      
MODEV    DC    A(MODE),C'GET/PUT:'                                              
CONCATV  DC    A(CONCAT),C'Y/N;Concatenate FB PUTs:'                            
SPLITV   DC    A(SPLIT),C'Y/N;Split messages by job:'                           
QMGRV    DC    A(QMGR),C';Queue manager:'                                       
QUEUEV   DC    A(QUEUE),C';Queue name:'                                         
QCORIDV  DC    A(QCORID),C';Correlation ID:'                                    
QMSGIDV  DC    A(QMSGID),C';Message ID:'                                        
         DC    X'FFFF'                                                          
         EJECT                                                                  
************************************************************                    
*        CALL MQ                                           *                    
************************************************************                    
                                                                                
MQCONNEC LA    R2,AMQBCONN         CONNECT                                      
         J     CALLMQ                                                           
MQOPEN   LA    R2,AMQBOPEN         OPEN                                         
         J     CALLMQ                                                           
MQPUT    LA    R2,AMQBPUTN         PUT                                          
         J     CALLMQ                                                           
MQGET    LA    R2,AMQBGETN         GET                                          
         J     CALLMQ                                                           
MQCOMMIT LA    R2,AMQBCOMM         COMMIT                                       
         J     CALLMQ                                                           
MQCLOSE  LA    R2,AMQBCLOS         CLOSE                                        
         J     CALLMQ                                                           
MQDISCON LA    R2,AMQBDISC         DISCONNECT                                   
         J     CALLMQ                                                           
                                                                                
************************************************************                    
* ENTRY  R2 POINTS TO PARAMETER STRUCTURE                  *                    
* RETURN COMPCODE CONTAINS THE MQ COMPLETION CODE          *                    
* REASON CONTAINS THE MQ REASON CODE                       *                    
* ERRORS RETURN TO XBASE EXCEPT MQGET WHICH RETURNS NEQ    *                    
************************************************************                    
                                                                                
CALLMQ   NTR1                                                                   
         SAM31                     SWITCH TO 31-BIT MODE                        
         L     RF,0(R2)            RF = A(MQ ROUTINE)                           
         LA    R3,24(R2)           R3 = A(PARAMETER LIST)                       
         CALL  (15),MF=(E,(R3))                                                 
         SAM24                     SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P+0(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODES          
         MVC   P+17(2),=C'OK      '                                             
*                                                                               
CALLMQ01 CLC   4(6,R2),=C'MQ_PUT'  IF PUT                                       
         JNE   CALLMQ02                                                         
         MVC   FULL,BUFFLEN        SAVE LENGTH                                  
         J     CALLMQ03                                                         
*                                                                               
CALLMQ02 CLC   4(6,R2),=C'MQ_GET'  OR GET                                       
         JNE   CALLMQ05                                                         
         MVC   FULL,DATALEN        SAVE LENGTH                                  
*                                                                               
CALLMQ03 EDIT  (B4,FULL),(6,P+7)                                                
         MVI   P+14,C'B'                                                        
         L     R1,FULL                                                          
         A     R1,BYTECNT          UPDATE BYTE COUNT WITH LENGTH                
         ST    R1,BYTECNT                                                       
         LA    R1,1                                                             
         A     R1,BYTECAL          UPDATE CALL COUNT                            
         ST    R1,BYTECAL                                                       
*                                                                               
CALLMQ05 CLC   COMPCODE,=A(MQCC_OK)                                             
         JNE   CALLMQ07                                                         
         CLC   4(6,R2),=C'MQ_GET'  IF GET OR PUT RETURN OK                      
         JE    CALLMQ40                                                         
         CLC   4(6,R2),=C'MQ_PUT'  NO MESSAGE JUST CARRY ON                     
         JE    CALLMQ40                                                         
         J     CALLMQ30            LOG MESSAGE FOR OTHERS                       
*                                                                               
CALLMQ07 MVC   P+17(8),=C'WARNING!'                                             
         CLC   COMPCODE,=A(MQCC_WARNING)                                        
         JE    CALLMQ10                                                         
         MVC   P+17(8),=C'*FAILED!'                                             
         CLC   COMPCODE,=A(MQCC_FAILED)                                         
         JE    CALLMQ10                                                         
         DC    H'0'                UNKNOWN COMPLETION CODE                      
*                                                                               
CALLMQ10 MVC   P+26(7),=C'REASON='                                              
         EDIT  REASON,(5,P+34),ZERO=NOBLANK                                     
         MVI   P+39,C':'                                                        
*                                                                               
         L     RF,=A(MQ_REASON_CODE_TABLE)                                      
CALLMQ20 CLI   0(RF),X'FF'         END OF TABLE?                                
         JNE   *+14                NO                                           
         MVC   P+41(22),=C'*** UNKNOWN REASON ***'                              
         J     CALLMQ30            REASON CODE NOT IN TABLE                     
*                                                                               
         CLC   REASON,0(RF)        FIND MATCH ON REASON CODE                    
         JE    *+12                GOT IT                                       
         LA    RF,28(RF)           BUMP TO NEXT TABLE ENTRY                     
         J     CALLMQ20                                                         
*                                                                               
         MVC   P+41(24),4(RF)      COPY INTO LOG MESSAGE                        
*                                                                               
CALLMQ30 OC    BYTECNT,BYTECNT     ANY BYTES PROCESSED                          
         JZ    CALLMQ35                                                         
*                                                                               
         MVC   PCOUNTS,SPACES                                                   
         MVC   PCOUNTS(13),=C'DATA TRANSFER'                                    
         MVC   PCOUNTS+20(31),=C'CALLS=          BYTES=         '               
         EDIT  (B4,BYTECAL),(9,PCOUNTS+26)                                      
         EDIT  (B4,BYTECNT),(9,PCOUNTS+42)                                      
         XC    BYTECNT,BYTECNT                                                  
         XC    BYTECAL,BYTECAL                                                  
         MVC   PCOUNTS+65(2),=C'//'                                             
         GOTO1 =V(DDWTO),DMCB,PCOUNTS,(X'80',0)                                 
*                                                                               
CALLMQ35 CLC   REASON,=F'2033'     2033 IS JUST EOF SO OK                       
         JNE   *+16                BUT MUST EXIT NEQ                            
         MVC   P+17(40),SPACES                                                  
         MVC   P+7(12),=C'EOF       OK'                                         
*                                                                               
         MVC   P+65(2),=C'//'                                                   
         GOTO1 =V(DDWTO),DMCB,P,(X'80',0)                                       
*                                                                               
CALLMQ40 CLC   COMPCODE,=A(MQCC_OK)     TEST FOR GOOD COMPLETION                
         JE    CALLMQ50                                                         
         CLC   4(6,R2),=C'MQ_GET'  ONLY GET ALLOWS NEQ RETURN                   
         JNE   XBASE               ALL OTHERS GO TO XBASE                       
         CLC   COMPCODE,=A(MQCC_OK)     TEST AGAIN TO GET NEQ                   
*                                                                               
CALLMQ50 XIT1                                                                   
         SPACE 2                                                                
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA AREAS HERE                                                     *         
***********************************************************************         
*                                                                               
$$DATA   LOCTR ,                                                                
*                                                                               
************************************************************                    
*        CARD OUTPUT AREAS SET WITH DEFAULTS               *                    
************************************************************                    
         SPACE 1                                                                
DDSIO    DC    C'DDSIO   '                                                      
TYPE     DC    C'DATA    '         DATA/VB/FB/XFER                              
MODE     DC    C'NONE    '         GET/PUT                                      
CONCAT   DC    C'N'                Y FOR CONCAT                                 
SPLIT    DC    C'Y'                Y FOR TRUNCATE                               
WIDTH    DC    F'80'               DEFAULT WIDTH=80                             
COMCOUNT DC    F'5000'             DEFAULT 5000                                 
MAXWAIT  DC    F'5'                                                             
*                                                                               
QMGR     DC    CL48' '                                                          
QUEUE    DC    CL48' '                                                          
*                                                                               
QCORID   DC    XL(L'MSGDESC_CORRELID)'00'                                       
QMSGID   DC    XL(L'MSGDESC_MSGID)'00'                                          
*                                                                               
************************************************************                    
*        DCBS LTORG                                        *                    
************************************************************                    
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
SPACES   DC    166C' '                                                          
INITREC  DC    X'00000000',C'**** START OF DATA ****     '                      
EOFREC   DC    X'FFFFFFFF',C'****  END OF DATA  ****     '                      
*                                                                               
TAPEIN   DCB   DSORG=PS,MACRF=GL,DDNAME=TAPEIN,EODAD=GOODBYE                    
TAPEINF  DCB   DDNAME=TAPEIN,MACRF=GM,DSORG=PS,EODAD=GOODBYEF                   
TAPEINV  DCB   DDNAME=TAPEIN,DSORG=PS,MACRF=(GL),RECFM=VB,             *        
               EODAD=GOODBYEV                                                   
*                                                                               
TAPEOUT  DCB   DSORG=PS,MACRF=PM,DDNAME=TAPEOUT                                 
TAPEOUTF DCB   DDNAME=TAPEOUT,MACRF=PM,DSORG=PS                                 
TAPEOUTV DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),RECFM=VB,            *        
               BLKSIZE=8200,LRECL=4004,BUFNO=2                                  
*                                                                               
         CMQA  LIST=YES,EQUONLY=NO                                              
*                                                                               
OBJDESC  CMQODA  LIST=YES  OBJECT DESCRIPTOR                                    
*                                                                               
MSGDESC  CMQMDA  LIST=YES  MESSAGE DESCRIPTOR                                   
         ORG     MSGDESC_FORMAT                                                 
         DC      CL8'MQSTR   '     MQFMT_STRING  FOR DATA FORMAT                
         ORG                                                                    
*                                                                               
PUTOPTS  CMQPMOA LIST=YES  PUT MESSAGE OPTIONS                                  
GETOPTS  CMQGMOA LIST=YES  GET MESSAGE OPTIONS                                  
         EJECT                                                                  
************************************************************                    
* PARAMETER LISTS FOR MQSERIES CALLS                       *                    
*  F    A(ROUTINE)                                         *                    
*  CL16 EBCDIC ROUTINE NAME                                *                    
*  XL1  FLAGS                                              *                    
*  XL3  SPARE                                              *                    
*  PARAMETERS (STANDARD IBM FORMAT)                        *                    
************************************************************                    
*                                                                               
AMQBCONN DC    A(0)                STRUCTURE                                    
         DC    CL16'MQ_CONNECT'                                                 
         DC    X'00'                                                            
         DC    XL3'000000'                                                      
         DC    X'00',AL3(QMGR)                                                  
         DC    X'00',AL3(HCONN)                                                 
         DC    X'00',AL3(COMPCODE)                                              
         DC    X'80',AL3(REASON)                                                
*                                                                               
AMQBOPEN DC    A(0)                STRUCTURE                                    
         DC    CL16'MQ_OPEN'                                                    
         DC    X'00'                                                            
         DC    XL3'000000'                                                      
         DC    X'00',AL3(HCONN)                                                 
         DC    X'00',AL3(OBJDESC)                                               
         DC    X'00',AL3(OPENOPT)                                               
         DC    X'00',AL3(HOBJ)                                                  
         DC    X'00',AL3(COMPCODE)                                              
         DC    X'80',AL3(REASON)                                                
*                                                                               
AMQBPUTN DC    A(0)                STRUCTURE                                    
         DC    CL16'MQ_PUT'                                                     
         DC    X'00'                                                            
         DC    XL3'000000'                                                      
         DC    X'00',AL3(HCONN)                                                 
         DC    X'00',AL3(HOBJ)                                                  
         DC    X'00',AL3(MSGDESC)                                               
         DC    X'00',AL3(PUTOPTS)                                               
         DC    X'00',AL3(BUFFLEN)                                               
         DC    X'00',AL3(BUFFER)                                                
         DC    X'00',AL3(COMPCODE)                                              
         DC    X'80',AL3(REASON)                                                
*                                                                               
AMQBGETN DC    A(0)                STRUCTURE                                    
         DC    CL16'MQ_GET'                                                     
         DC    X'00'                                                            
         DC    XL3'000000'                                                      
         DC    X'00',AL3(HCONN)                                                 
         DC    X'00',AL3(HOBJ)                                                  
         DC    X'00',AL3(MSGDESC)                                               
         DC    X'00',AL3(GETOPTS)                                               
         DC    X'00',AL3(BUFFLEN)                                               
         DC    X'00',AL3(BUFFER)                                                
         DC    X'00',AL3(DATALEN)                                               
         DC    X'00',AL3(COMPCODE)                                              
         DC    X'80',AL3(REASON)                                                
*                                                                               
AMQBCOMM DC    A(0)                STRUCTURE                                    
         DC    CL16'MQ_COMMIT'                                                  
         DC    X'00'                                                            
         DC    XL3'000000'                                                      
         DC    X'00',AL3(HCONN)                                                 
         DC    X'00',AL3(COMPCODE)                                              
         DC    X'80',AL3(REASON)                                                
*                                                                               
AMQBCLOS DC    A(0)                STRUCTURE                                    
         DC    CL16'MQ_CLOSE'                                                   
         DC    X'00'                                                            
         DC    XL3'000000'                                                      
         DC    X'00',AL3(HCONN)                                                 
         DC    X'00',AL3(HOBJ)                                                  
         DC    X'00',AL3(CLOSEOPT)                                              
         DC    X'00',AL3(COMPCODE)                                              
         DC    X'80',AL3(REASON)                                                
*                                                                               
AMQBDISC DC    A(0)                STRUCTURE                                    
         DC    CL16'MQ_DISCONNECT'                                              
         DC    X'00'                                                            
         DC    XL3'000000'                                                      
         DC    X'00',AL3(HCONN)                                                 
         DC    X'00',AL3(COMPCODE)                                              
         DC    X'80',AL3(REASON)                                                
*                                                                               
ENTRYPTS DS    0F                                                               
         DC    Y((ENTRYLSQ-ENTRYSTQ)/60)   NUMBER OF TABLE ENTRIES              
         DC    H'60'                       MUST REMAIN AS 60                    
ENTRYSTQ EQU   *                                                                
*                                                                               
CSQBCLOS DC    CL8'CSQBCLOS'                                                    
         DC    XL52'00'                                                         
CSQBCOMM DC    CL8'CSQBCOMM'                                                    
         DC    XL52'00'                                                         
CSQBCONN DC    CL8'CSQBCONN'                                                    
         DC    XL52'00'                                                         
CSQBDISC DC    CL8'CSQBDISC'                                                    
         DC    XL52'00'                                                         
CSQBGET  DC    CL8'CSQBGET'                                                     
         DC    XL52'00'                                                         
CSQBOPEN DC    CL8'CSQBOPEN'                                                    
         DC    XL52'00'                                                         
CSQBPUT  DC    CL8'CSQBPUT'                                                     
         DC    XL52'00'                                                         
*                                                                               
ENTRYLSQ EQU   *                                                                
         EJECT                                                                  
************************************************************                    
*        MQSERIES CALL PARAMETERS                          *                    
************************************************************                    
*                                                                               
HCONN    DS      F         MQ QMGR CONNECTION HANDLE                            
HOBJ     DS      F         OBJECT HANDLE                                        
OPENOPT  DS      F         MQOPEN OPTIONS                                       
CLOSEOPT DS      F         MQCLOSE OPTIONS                                      
COMPCODE DS      F         COMPLETION CODE                                      
REASON   DS      F         QUALIFIES COMPLETION CODE                            
BUFFLEN  DS      F         LENGTH OF MQBUFFER AREA                              
DATALEN  DS      F         LENGTH OF THE MESSAGE                                
*                                                                               
IOL      DS      F                                                              
BUFFER   DS      CL32768                                                        
         EJECT                                                                  
       ++INCLUDE DDMQREASON                                                     
         EJECT                                                                  
$$CODE   LOCTR   ,                                                              
*************************************************************                   
*        WORKING STORAGE DC                                 *                   
*************************************************************                   
         SPACE 1                                                                
         DS    0D                                                               
WORKAREA DC    60000D'00'                                                       
         EJECT                                                                  
************************************************************                    
*        WORKING STORAGE                                   *                    
************************************************************                    
         SPACE 1                                                                
WORKD    DSECT                                                                  
SAVERD   DS    F                                                                
SAVERE   DS    F                                                                
DMWORK   DS    12D                                                              
DMCB     DS    6F                                                               
*                                                                               
         DS    0F                  PARM BLOCK FOR VALCARD                       
VCCB     DS    0XL24               VALCARD CONTROL BLOCK - NOT DMCB             
VCPCARD  DS    F                   A(CARD)                                      
VCPTABLE DS    F                   A(CARDTAB)                                   
VCPROUT  DS    F                   RET - A(ROUTINE) TO CALL                     
VCPVALUE DS    F                   RET - A(VALUE FOR ROUTINE)                   
VCPLEN   DS    F                   RET - LEN OF VALUE                           
VCPNEXT  DS    F                   RET - POINTER TO NEXT - REPEAT CALL          
*                                                                               
DUB      DS    D                                                                
DUB2     DS    D                                                                
PRNTDUB  DS    D                                                                
COUNTER  DS    F                                                                
BYTECNT  DS    F                                                                
BYTECAL  DS    F                                                                
SEQN     DS    F                                                                
WAITS    DS    F                                                                
RECORD   DS    CL255                                                            
CARD     DS    CL80                                                             
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
FLAG     DS    X                                                                
RETCODE  DS    X                                                                
PRNTTIME DS    CL9                                                              
WORK     DS    CL256                                                            
PLINE    DS    CL166                                                            
P        DS    CL166                                                            
PCOUNTS  DS    CL166                                                            
WORKX    EQU   *                                                                
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
*PERVALD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DDMQUTIL  05/20/20'                                      
         END                                                                    
