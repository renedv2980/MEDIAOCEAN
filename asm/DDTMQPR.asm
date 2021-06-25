*          DATA SET DDTMQPR    AT LEVEL 068 AS OF 05/01/02                      
DDTMQPR TITLE 'MQSERIES TEST PROGRAM - PUT REQUEST MESSAGES TO QUEUE'           
*PHASE DDTMQPRA                                                                 
***********************************************************************         
*                                                                     *         
* MODULE NAME    : DDTMQPR                                            *         
*                                                                     *         
* ENVIRONMENT    : MVS Batch; BAL                                     *         
*                                                                     *         
* DESCRIPTION    : MQ Series test program to get mesages from         *         
*                  specified queue.                                   *         
*                                                                     *         
* FUNCTION       : This program gets messages from specified queue    *         
*                  in the specified queue manager                     *         
*                                                                     *         
*                  The program processes the first 80 bytes only of   *         
*                  each message. It uses the BROWSE option on the     *         
*                  MQGET call to ensure that data is not lost         *         
*                                                                     *         
* PARAMETERS     : Standard MVS passing of parameters.                *         
*                  This program expects two parameters delimited      *         
*                  by a comma                                         *         
*                                                                     *         
*                  PARM1 = Name of the queue manager to connect to.   *         
*                  PARM2 = Name of the queue to be printed            *         
*                                                                     *         
* REGISTERS      :                                                    *         
*                  R0  - Work register                                *         
*                  R1  - Work register                                *         
*                  R2  - Work register                                *         
*                  R3  - Work register                                *         
*                  R4  - Work register                                *         
*                  R5  - Length for executes                          *         
*                  R6  - Branch for main routines                     *         
*                  R7  - Printer DSECTS                               *         
*                  R8  - Work register                                *         
*                  R9  - 3rd Base register                            *         
*                  R10 - 2nd Base register                            *         
*                  R11 - Base register                                *         
*                  R12 - Work area DSECT                              *         
*                  R13 - Address of register save area                *         
*                  R14 - Return address/work register                 *         
*                  R15 - Work register/return code                    *         
*                                                                     *         
***********************************************************************         
*                                                                     *         
* EXECUTABLE MACROS                                                   *         
*                                                                     *         
*    CALL    -  To execute MQCONN, MQOPEN, MQGET, MQCLOSE, MQDISC     *         
*              -  Needs ',MF=(E,CALLLIST)' to enable code to be       *         
*                 reentrant                                           *         
*              -  Needs ',VL' so diagnostic facilities                *         
*                 know how long the parameter list is.                *         
*                                                                     *         
*    SAVE    - To save register contents                              *         
*                                                                     *         
*    GETMAIN - To get storage                                         *         
*                                                                     *         
*    TIME    - To get current date                                    *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
********************************************************************            
*                                                                               
*                         Program logic                                         
*                         -------------                                         
*        DDTMQG1 CSECT                                                          
*        -------------                                                          
*                                                                               
*           Save registers                                                      
*           Establish addressability                                            
*           Get working storage                                                 
*           Branch to MAIN                                                      
*                                                                               
*        MAIN                                                                   
*        ----                                                                   
*           Call MAININIT                                                       
*           Call MAINPARM                                                       
*           Call MAINCONN                                                       
*           Call MAINMSGS                                                       
*           Call MAINDISC                                                       
*           Call ENDPROG                                                        
*                                                                               
*        MAININIT                                                               
*        --------                                                               
*           Open SYSPRINT file                                                  
*           Get and initialize storage                                          
*                                                                               
*        MAINPARM                                                               
*        --------                                                               
*           Obtain the input data from parameters passed                        
*                                                                               
*           If the name of the queue manager is missing                         
*              Build a warning message and move it into data line               
*              Print the line (using PRTLINE)                                   
*              Continue (using default queue manager name)                      
*           End-if                                                              
*                                                                               
*           If the name of the queue is missing                                 
*              Build an error message and move it into data line                
*              Print the line (using PRTLINE)                                   
*              Branch to ENDPROG                                                
*           End-if                                                              
*                                                                               
*        MAINCONN                                                               
*        --------                                                               
*           Connect to the queue manager                                        
*           If an error occurs                                                  
*              Build an error message and move it into data line                
*              Print the line (using PRTLINE)                                   
*              Branch to Exit2                                                  
*           End-if                                                              
*                                                                               
*        MAINMSGS                                                               
*        --------                                                               
*           Open the queue                                                      
*           If an error occurs                                                  
*              Build an error message and move it into data line                
*              Print the line (using PRTLINE)                                   
*              Branch to Exit1                                                  
*           End-if                                                              
*                                                                               
*           Do while no error                                                   
*                                                                               
*              Add 1 to relative message number                                 
*              Move message into print line (maximum 80 bytes)                  
*              Print the line (using PRTLINE)                                   
*                                                                               
*              Get the message (using BROWSE-NEXT option)                       
*                                                                               
*           End-do                                                              
*                                                                               
*           When an error occurs                                                
*              If no more messages                                              
*                 Do nothing                                                    
*              else                                                             
*                 Build an error message and move it into data line             
*                 Print the line (using PRTLINE)                                
*              End-if                                                           
*           End-if                                                              
*                                                                               
*           Close the queue                                                     
*           If an error occurs                                                  
*              Build an error message and move it into data line                
*              Print the line (using PRTLINE)                                   
*           End-if                                                              
*                                                                               
*        MAINDISC                                                               
*        --------                                                               
*           Disconnect from the queue manager                                   
*           If an error occurs                                                  
*              Build an error message and move it into data line                
*              Print the line (using PRTLINE)                                   
*           End-if                                                              
*                                                                               
*        ENDPROG                                                                
*        -------                                                                
*           Print end report message                                            
*           Close output file                                                   
*           Restore registers                                                   
*           Return to caller                                                    
*                                                                               
*        ERRCODE                                                                
*        -------                                                                
*           Convert compcode to displayable format                              
*           Convert reason to displayable format                                
*           Print the error message                                             
*           Return to calling section                                           
*                                                                               
***********************************************************************         
         EJECT                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE TIMBER                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE SCANNER                                                                
*INCLUDE GETDAY                                                                 
*INCLUDE GETRET                                                                 
*INCLUDE GETFACT                                                                
         EJECT                                                                  
         PRINT NOGEN                                                            
         EJECT                                                                  
**********************************************************************          
* START OF PROGRAM                                                   *          
**********************************************************************          
DDTMQPR  CSECT                                                                  
DDTMQPR  AMODE 24                                                               
         NBASE WORKX-WORKD,**MQR1**,WORK=A(WORKC),RA,CLEAR=YES                  
         USING WORKD,RC                                                         
*                                                                               
         ST    R1,PARMSAVE         A(R1 PARAMETERS FROM ATTACH)                 
         ST    RB,RBSAVE                                                        
*                                                                               
         L     R7,VCPRINT                                                       
         USING DPRINT,R7                                                        
         MVC   TITLE(20),=CL20'DDMQPT2'                                         
*                                                                               
         B     MAIN                         Branch to MAIN process              
         EJECT                                                                  
********************************************************************            
*  SECTION NAME : MAIN                                             *            
*                                                                  *            
*  FUNCTION     : Controls flow of program                         *            
*                                                                  *            
*  CALLED BY    : DDTMQG2  CSECT                                   *            
*                                                                  *            
*  CALLS        : MAININIT, MAINPARM, MAINCONN, MAINMSGS, MAINDISC *            
*                 ENDPROG                                          *            
*                                                                  *            
********************************************************************            
MAIN     DS    0H                                                               
         MVC   MSGDATA,SPACES                                                   
         BAS   R6,MAININIT                  Initialize storage                  
         BAS   R6,MAINPARM                  Reads the parms                     
         BAS   R6,MAINCARD                  Reads JCL cards                     
         BAS   R6,MAINCONN                  Connect to qmgr                     
         BAS   R6,MAINMSGS                  Put messages to queue               
         BAS   R6,MAINDISC                  Disconnect from qmgr                
         B     ENDPROG                      Terminate program                   
*                                                                               
         EJECT                                                                  
********************************************************************            
*  SECTION NAME : MAININIT                                         *            
*                                                                  *            
*  FUNCTION     : Performs initialization                          *            
*                                                                  *            
*  CALLED BY    : MAIN                                             *            
*                                                                  *            
*  CALLS        : PRINTHDR                                         *            
*                                                                  *            
*  RETURN       : To Register 6                                    *            
*                                                                  *            
********************************************************************            
MAININIT DS    0H                                                               
*                                                                               
         MVC   MSGDATA,SPACES                                                   
*                                                                               
         TIME  DEC,ZONE=LT                                                      
*                                                                               
         ST    R1,TIMEDATE                  Julian date in packed dec           
         UNPK  DATECONV,TIMEDATE+1(3)       To unsigned zoned                   
         MVZ   DATECONV+5(1),DATECONV+4     Decimal                             
         MVC   DATEYR,DATE_YR               Move date - year                    
         MVC   DATEJN,DATE_JN               Move date - Julian                  
*                                                                               
         XR    R0,R0                        Zero register                       
         ST    R0,PAGENUM                   Zero page number                    
         ST    R0,MSGNUM                    Zero message number                 
         ST    R0,LINENUM                   Zero line number                    
         ST    R0,EXITCODE                  Default return code                 
         BR    R6                                                               
*                                                                               
         EJECT                                                                  
********************************************************************            
*  SECTION NAME : MAINPARM                                         *            
*                                                                  *            
*  FUNCTION     : Read the parameters passed to the program, and   *            
*                 incorrect number of parameters passed            *            
*                                                                  *            
*  CALLED BY    : MAIN                                             *            
*                                                                  *            
*  CALLS        : PRINTHDG, PRTLINE                                *            
*                                                                  *            
*  RETURN       : Normal to Register 6                             *            
*                 Error  to ENDPROG                                *            
*                                                                  *            
********************************************************************            
MAINPARM DS    0H                                                               
         MVI   MQMNAME,X'40'                Space out first byte                
         MVC   MQMNAME+1(L'MQMNAME-1),MQMNAME  and initialize                   
         MVI   MQMQUEUE,X'40'               Space out first byte                
         MVC   MQMQUEUE+1(L'MQMQUEUE-1),MQMQUEUE  and initialize                
*                                                                               
         LA    RE,MQMPMSG                                                       
         ICM   RF,15,=AL4(L'MQMPMSG)                                            
         LA    R0,*                                                             
         LA    R1,C' '                                                          
         MVCL  RE,R0                                                            
         XC    MQMPMSGL,MQMPMSGL                                                
*                                                                               
         L     R1,PARMSAVE                  Address of parm list                
         L     R1,0(R1)                     Address of first parm               
         LH    R5,0(R1)                     Length of parm                      
         STH   R5,PARMLEN                   Save away                           
         LTR   R5,R5                        any data passed                     
         BZ    NOPARMS                      No names passed                     
*                                                                               
TRANPARM DS    0H                                                               
         LA    R3,2(R1)                     Advance to start of parm            
         CLI   0(R3),C','                                                       
         BNE   TPAR010                                                          
         MVI   HDR2_QMN,C' '                Space out first byte                
         MVC   HDR2_QMN+1(L'HDR2_QMN-1),HDR2_QMN  and initialize                
         MVC   MSGDATA,INF2                 Move in the message                 
         BAS   RE,PRTLINE                   and print the line                  
         B     TPAR030                                                          
TPAR010  LA    R4,MQMNAME                   Address for target                  
         LA    R2,L'MQMNAME                                                     
TPAR020  MVC   0(1,R4),0(R3)                                                    
         LA    R4,1(R4)                                                         
         LA    R3,1(R3)                                                         
         BCT   R2,*+8                                                           
         B     NOPARMS                                                          
         BCT   R5,*+8                                                           
         B     NOPARM2                                                          
         CLI   0(R3),C','                                                       
         BNE   TPAR020                                                          
TPAR030  LA    R4,MQMQUEUE                  Address for target                  
         LA    R2,L'MQMQUEUE                                                    
         LA    R3,1(R3)                                                         
         BCTR  R5,0                                                             
TPAR040  MVC   0(1,R4),0(R3)                                                    
         LA    R4,1(R4)                                                         
         LA    R3,1(R3)                                                         
         BCT   R2,*+8                                                           
         B     NOPARMS                                                          
         BCT   R5,*+8                                                           
         B     NOPARMS                                                          
         CLI   0(R3),C','                                                       
         BNE   TPAR040                                                          
TPAR050  LA    R4,MQMPMSG                                                       
         LA    R2,L'MQMPMSG                                                     
         LA    R3,1(R3)                                                         
         BCTR  R5,0                                                             
         STCM  R5,15,MQMPMSGL                                                   
TPAR060  MVC   0(1,R4),0(R3)                                                    
         LA    R4,1(R4)                                                         
         LA    R3,1(R3)                                                         
         BCT   R2,*+8                                                           
         B     NOPARMS                                                          
         BCT   R5,*+8                                                           
         B     ENDPARMS                                                         
         CLI   0(R3),C','                                                       
         BNE   TPAR060                                                          
         B     NOPARMS                                                          
*                                                                               
NOPARM2  DS    0H                                                               
         MVC   MSGDATA,INF3                 Move in the message                 
         BAS   RE,PRTLINE                   and print the line                  
         LA    R0,4                         Set return code                     
         ST    R0,EXITCODE                  ready for exit                      
         B     ENDPROG                      End the program                     
*                                                                               
NOPARMS  DS    0H                                                               
         MVC   MSGDATA,INF1                 Move message to buffer              
         BAS   R8,PRTLINE                   Write the record                    
         LA    R0,4                         Set return code                     
         ST    R0,EXITCODE                  ready for exit                      
         B     ENDPROG                      End the program                     
*                                                                               
ENDPARMS DS    0H                                                               
         BR    R6                           Return to caller                    
         EJECT                                                                  
********************************************************************            
*  SECTION NAME : MAINCARD                                         *            
*                                                                  *            
*  FUNCTION     : Read the parameters passed to the program        *            
*                 through JCL SYSIN CARDS.                         *            
*                                                                  *            
*  CALLED BY    : MAIN                                             *            
*                                                                  *            
*  CALLS        : PRINTHDG, PRTLINE                                *            
*                                                                  *            
*  RETURN       : Normal to Register 6                             *            
*                 Error  to ENDPROG                                *            
*                                                                  *            
********************************************************************            
MAINCARD DS    0H                                                               
         LA    RF,MQMPMSG                                                       
         ST    RF,AMQMPMSG                                                      
         XC    MQMPMSGL,MQMPMSGL                                                
*                                                                               
MCLP1    GOTO1 VCARDS,DMCB,P,=C'RE00'                                           
         CLC   =C'/*',P            IF END OF JCL                                
         BE    ENDCARD                                                          
*                                                                               
         L     RF,AMQMPMSG                                                      
         LA    RE,P                                                             
         SR    R1,R1                                                            
         LA    R0,70                                                            
MCLP010  CLC   0(5,RE),=CL5'*END*'                                              
         BE    MCLP020                                                          
         CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,MCLP010                                                       
         DC    H'0'                                                             
MCLP020  STCM  RF,15,AMQMPMSG                                                   
         ICM   RE,15,MQMPMSGL                                                   
         AR    RE,R1                                                            
         STCM  RE,15,MQMPMSGL                                                   
         B     MCLP1                                                            
*                                                                               
ENDCARD  DS    0H                                                               
         BR    R6                           Return to caller                    
*                                                                               
         EJECT                                                                  
********************************************************************            
*  SECTION NAME : MAINCONN                                         *            
*                                                                  *            
*  FUNCTION     : Connect to the queue manager                     *            
*                                                                  *            
*  CALLED BY    : MAIN                                             *            
*                                                                  *            
*  CALLS        : ERRCODE                                          *            
*                                                                  *            
*  RETURN       : Normal to Register 6                             *            
*                 Error  to ENDPROG                                *            
*                                                                  *            
********************************************************************            
MAINCONN DS    0H                                                               
         XC    HCONN,HCONN                  Null connection handle              
*                                                                               
         LOAD  EP=CSQBCONN                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LR    RF,R0                                                            
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
*                                                                               
         CALL  (15),                                                   X        
               (MQMNAME,                                               X        
               HCONN,                                                  X        
               COMPCODE,                                               X        
               REASON),                                                X        
               MF=(E,PARMLIST),VL                                               
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                                                             
*                                                                               
         DELETE EP=CSQBCONN                                                     
*                                                                               
         LA    R0,MQCC_OK                   Expected compcode                   
         C     R0,COMPCODE                  As expected ?                       
         BER   R6                           Yes .. return to caller             
*                                                                               
         MVC   INF4_TYP,=CL10'CONNECT   '                                       
         BAS   RE,ERRCODE                   Translate error                     
         LA    R0,8                         Set exit code                       
         ST    R0,EXITCODE                  to 8                                
         B     ENDPROG                      End the program                     
*                                                                               
         EJECT                                                                  
********************************************************************            
*  SECTION NAME : MAINMSGS                                         *            
*                                                                  *            
*  FUNCTION     : Read the messages from the queue                 *            
*                                                                  *            
*  CALLED BY    : MAIN                                             *            
*                                                                  *            
*  CALLS        : ERRCODE, PRTLINE                                 *            
*                                                                  *            
*  RETURN       : to Register 6                                    *            
*                                                                  *            
********************************************************************            
MAINMSGS DS    0H                                                               
         LA    R0,MQOT_Q                    Object is a queue                   
         ST    R0,OBJDESC_OBJECTTYPE        In object type field                
         MVC   OBJDESC_OBJECTNAME,MQMQUEUE  Move queue name                     
         LA    R0,MQOO_OUTPUT               Indicate open is                    
         ST    R0,OPTIONS                   Output only                         
*                                                                               
         LOAD  EP=CSQBOPEN                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LR    RF,R0                                                            
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
*                                                                               
         CALL  (15),                                                   X        
               (HCONN,                                                 X        
               OBJDESC,                                                X        
               OPTIONS,                                                X        
               HOBJ,                                                   X        
               COMPCODE,                                               X        
               REASON),                                                X        
               MF=(E,PARMLIST),VL                                               
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                                                             
*                                                                               
         DELETE EP=CSQBOPEN                                                     
*                                                                               
         LA    R0,MQCC_OK                   Expected compcode                   
         C     R0,COMPCODE                  As expected?                        
         BE    MSGSORPL                     Yes .. continue                     
         MVC   INF4_TYP,=CL10'OPEN      '                                       
         BAS   RE,ERRCODE                   Translate error                     
         LA    R0,8                         Set exit code                       
         ST    R0,EXITCODE                  to 8                                
         BR    R6                           Return to disconnect from           
*                                           qmgr and terminate program          
MSGSORPL LA    R0,MQOT_Q                    Object is a queue                   
         ST    R0,OBJDESC_OBJECTTYPE        In object type field                
         MVC   OBJDESC_OBJECTNAME,MODELRQ   Move REPLY Q NAME                   
*                                                                               
         LOAD  EP=CSQBOPEN                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LR    RF,R0                                                            
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
*                                                                               
         CALL  (15),                                                   X        
               (HCONN,                                                 X        
               OBJDESC,                                                X        
               OPTIONS,                                                X        
               HROBJ,                                                  X        
               COMPCODE,                                               X        
               REASON),                                                X        
               MF=(E,PARMLIST),VL                                               
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                                                             
*                                                                               
         DELETE EP=CSQBOPEN                                                     
*                                                                               
         LA    R0,MQCC_OK                   Expected compcode                   
         C     R0,COMPCODE                  As expected?                        
         BE    MSGSINIT                     Yes .. continue                     
         DC    H'0'                                                             
         MVC   INF4_TYP,=CL10'OPEN RPLYQ'                                       
         BAS   RE,ERRCODE                   Translate error                     
         LA    R0,8                         Set exit code                       
         ST    R0,EXITCODE                  to 8                                
         BR    R6                           Return to disconnect from           
*                                                                               
MSGSINIT DS    0H                                                               
         MVC   DYNRQ,OBJDESC_OBJECTNAME                                         
*                                                                               
*-------------------------------------------------------------------*           
* Code segment which gets the message from the requested queue.     *           
*-------------------------------------------------------------------*           
         DS    0H                                                               
         XC    MSGDESC_CORRELID,MSGDESC_CORRELID  Null correlation id           
         XC    MSGDESC_MSGID,MSGDESC_MSGID        Null message id               
         MVC   MSGDESC_REPLYTOQ,DYNRQ                                           
         MVC   MSGDESC_REPLYTOQMGR,MQMNAME                                      
         MVC   MSGDESC_FORMAT,MQFMT_STRING                                      
*                                                                               
         LOAD  EP=CSQBPUT                                                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LR    RF,R0                                                            
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
*                                                                               
         CALL  (15),                                                   X        
               (HCONN,                                                 X        
               HOBJ,                                                   X        
               MSGDESC,                                                X        
               PUTMSGOPTS,                                             X        
               MQMPMSGL,                                               X        
               MQMPMSG,                                                X        
               COMPCODE,                                               X        
               REASON),                                                X        
               MF=(E,PARMLIST),VL                                               
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                                                             
*                                                                               
         DELETE EP=CSQBPUT                                                      
*                                                                               
         LA    R0,MQCC_OK                   Load compcode MQCC_OK               
         C     R0,COMPCODE                  As expected?                        
         BE    MSGSPRNT                     Yes .. print message                
*                                                                               
         LA    R0,MQCC_WARNING              Load compcode MQCC_WARNING          
         C     R0,COMPCODE                  As expected                         
         BE    MSGSPRNT                     Yes .. print message                
*                                                                               
         LA    R0,MQRC_NO_MSG_AVAILABLE     No more message?                    
         C     R0,REASON                    Yes .. then close                   
         BE    MSGSCLOS                     Otherwise must be                   
         B     MSGSERR                      an error                            
*                                                                               
MSGSPRNT DS    0H                                                               
         LA    R0,MQRC_TRUNCATED_MSG_ACCEPTED Was the warning because           
         C     R0,REASON                    message was too long?               
         BE    GETMSGS                      Yes .. get the message              
         LA    R0,MQRC_NONE                 Was it ok?                          
         C     R0,REASON                                                        
         BE    GETMSGS                      Yes .. get the message              
         B     MSGSERR                      Its unacceptable                    
*                                                                               
*-------------------------------------------------------------------*           
* Code segment which prints the message                             *           
*-------------------------------------------------------------------*           
GETMSGS  DS    0H                                                               
         MVC   REP2_MID,MSGDESC_MSGID                                           
         MVC   REP2_CID,MSGDESC_CORRELID                                        
         MVC   MSGDATA,REP2                 Move in the message                 
         BAS   RE,PRTLINE                   Go and print line                   
*                                                                               
*-------------------------------------------------------------------*           
* Code segment closes the queue                                     *           
*-------------------------------------------------------------------*           
MSGSCLOS DS    0H                                                               
         LA    R0,MQCO_NONE                 Indicate normal close               
         ST    R0,OPTIONS                   of the queue                        
*                                                                               
         LOAD  EP=CSQBCLOS                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LR    RF,R0                                                            
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
*                                                                               
         CALL  (15),                                                   X        
               (HCONN,                                                 X        
               HROBJ,                                                  X        
               OPTIONS,                                                X        
               COMPCODE,                                               X        
               REASON),                                                X        
               MF=(E,PARMLIST),VL                                               
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                                                             
*                                                                               
         DELETE EP=CSQBCLOS                                                     
*                                                                               
         LA    R0,MQCC_OK                   Expected compcode                   
         C     R0,COMPCODE                  As expected?                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R0,MQCO_NONE                 Indicate normal close               
         ST    R0,OPTIONS                   of the queue                        
*                                                                               
         LOAD  EP=CSQBCLOS                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LR    RF,R0                                                            
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
*                                                                               
         CALL  (15),                                                   X        
               (HCONN,                                                 X        
               HOBJ,                                                   X        
               OPTIONS,                                                X        
               COMPCODE,                                               X        
               REASON),                                                X        
               MF=(E,PARMLIST),VL                                               
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                                                             
*                                                                               
         DELETE EP=CSQBCLOS                                                     
*                                                                               
         LA    R0,MQCC_OK                   Expected compcode                   
         C     R0,COMPCODE                  As expected?                        
         BER   R6                           Yes .. continue                     
         MVC   INF4_TYP,=CL10'CLOSE     '                                       
         BAS   RE,ERRCODE                   Translate error                     
         LA    R0,8                         Set exit code                       
         ST    R0,EXITCODE                  To 8                                
         BR    R6                           Return to caller                    
*                                                                               
MSGSERR  DS    0H                                                               
         MVC   INF4_TYP,=CL10'GET       '                                       
         BAS   RE,ERRCODE                   Translate error                     
         LA    R0,8                         Set exit code                       
         ST    R0,EXITCODE                  To 8                                
         BR    R6                           Return to caller                    
*                                                                               
         EJECT                                                                  
********************************************************************            
*  SECTION NAME : MAINDISC                                         *            
*                                                                  *            
*  FUNCTION     : Disconnect from the queue manager                *            
*                                                                  *            
*  CALLED BY    : MAIN                                             *            
*                                                                  *            
*  CALLS        : ERRCODE                                          *            
*                                                                  *            
*  RETURN       : to Register 6                                    *            
*                                                                  *            
********************************************************************            
MAINDISC DS    0H                                                               
         LOAD  EP=CSQBDISC                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LR    RF,R0                                                            
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
*                                                                               
         CALL  (15),                                                   X        
               (HCONN,                                                 X        
               COMPCODE,                                               X        
               REASON),                                                X        
               MF=(E,PARMLIST),VL                                               
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                                                             
*                                                                               
         DELETE EP=CSQBDISC                                                     
*                                                                               
         LA    R0,MQCC_OK                   Expected compcode                   
         C     R0,COMPCODE                  As expected?                        
         BER   R6                           Yes .. continue                     
         MVC   INF4_TYP,=CL10'DISCONNECT'                                       
         BAS   RE,ERRCODE                   Translate error                     
         LA    R0,8                         Set exit code                       
         ST    R0,EXITCODE                  To 8                                
         BR    R6                           Return to caller                    
*                                                                               
         EJECT                                                                  
********************************************************************            
*  SECTION NAME : ENDPROG                                          *            
*                                                                  *            
*  FUNCTION     : Program termination                              *            
*                                                                  *            
*  CALLED BY    : MAIN                                             *            
*                                                                  *            
*  CALLS        : PRTLINE                                          *            
*                                                                  *            
*  RETURN       : leaves program                                   *            
*                                                                  *            
********************************************************************            
ENDPROG  DS    0H                                                               
         MVC   SKIPLINE,TWOLINES            Skip a line                         
         MVC   MSGDATA,INF0                 End report message                  
         BAS   RE,PRTLINE                   Go and print line                   
*                                                                               
MXBASE   XBASE RC=EXITCODE,RL=1             RETURN TO MVS                       
*                                                                               
         EJECT                                                                  
*********************************************************************           
* SUBROUTINES                                                       *           
*********************************************************************           
         EJECT                                                                  
********************************************************************            
*  SECTION NAME : ERRCODE                                          *            
*                                                                  *            
*  FUNCTION     : Print an error message including compcode and    *            
*                 reason                                           *            
*                                                                  *            
*  CALLED BY    : MAINCONN, MAINMSGS, MAINDISC                     *            
*                                                                  *            
*  CALLS        : PRTLINE                                          *            
*                                                                  *            
*  RETURN       : to Register 7                                    *            
*                                                                  *            
********************************************************************            
ERRCODE  NTR1                                                                   
         L     R0,COMPCODE                  Translate compcode                  
         CVD   R0,WORKDWRD                  To packed decimal                   
         UNPK  CONVAREA,WORKDWRD+4(4)       Convert to zoned decimal            
         MVZ   CONVAREA+7(1),CONVAREA+6     Make it displayable                 
         MVC   INF4_CC,CONVAREA+4           Move to display area                
*                                                                               
         L     R0,REASON                    Translate reason                    
         CVD   R0,WORKDWRD                  To packed decimal                   
         UNPK  CONVAREA,WORKDWRD+4(4)       Convert to zoned decimal            
         MVZ   CONVAREA+7(1),CONVAREA+6     Make it displayable                 
         MVC   INF4_RC,CONVAREA+4           Move to display area                
*                                                                               
         L     R1,LINENUM                   Current line number                 
         LA    R1,1(R1)                     Add one to line number              
         ST    R1,LINENUM                   Save again                          
         MVC   MSGDATA,INF4                 Move in the message                 
         BAS   RE,PRTLINE                   Print the line                      
         XIT1                               Return to caller                    
         EJECT                                                                  
********************************************************************            
*  SECTION NAME : PRTLINE                                          *            
*                                                                  *            
*  FUNCTION     : Print a line of data                             *            
*                                                                  *            
*  CALLED BY    : MAINCONN, MAINMSGS, MAINDISC                     *            
*                                                                  *            
*  CALLS        : PRTLINE                                          *            
*                                                                  *            
*  RETURN       : XIT1                                             *            
*                                                                  *            
********************************************************************            
PRTLINE  NTR1                                                                   
         MVC   P(L'MSGDATA),MSGDATA                                             
         LA    RE,PLIN010                                                       
         BSM   0,RE                                                             
PLIN010  EQU   *                                                                
         GOTO1 VPRINTER                                                         
         LA    RE,PLIN020                                                       
         O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
PLIN020  EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
* CONSTANTS, DATA STRUCTURES                                        *           
*********************************************************************           
SAVEAREA DS    9D                           Save area                           
PARMSAVE DS    F                            Save area for parms                 
NEWPAGE  DC    C'1'                         New page indicator                  
ONELINE  DC    C' '                         Write next line                     
TWOLINES DC    C'-'                         Write after advance 1               
MSGCLEAR DC    CL80' '                      Blank out message                   
MAXLINES DC    F'60'                        Max lines per page                  
ZEROREC  DC    F'0'                         Check for zero length               
*&&UK                                                                           
MODELRQ  DC    CL48'MQB.REPLY.MODELQ'                                           
*&&                                                                             
*&&US                                                                           
MODELRQ  DC    CL48'MQ5T.MQB.REPLY.MODELQ'                                      
*&&                                                                             
DYNRQ    DC    CL48' '                                                          
*                                                                               
OBJDESC    CMQODA  DSECT=NO,LIST=NO         Object descriptor                   
MSGDESC    CMQMDA  DSECT=NO,LIST=NO         Message descriptor                  
PUTMSGOPTS CMQPMOA DSECT=NO,LIST=NO         Get message options                 
*                                                                               
         SPACE 4                                                                
*********************************************************************           
* EXECUTES                                                          *           
*********************************************************************           
ONLYPARM MVC   0(*-*,R4),1(R1)                                                  
MOVEPARM MVC   0(*-*,R4),0(R3)                                                  
MOVEDATA MVC   0(*-*,R4),0(R3)                                                  
TRANSCAN TRT   0(*-*,R3),TRANTAB                                                
*                                                                               
         SPACE 4                                                                
*********************************************************************           
* TRANSLATE TABLE TO SCAN FOR DELIMETER.                            *           
*********************************************************************           
TRANTAB  DS   0CL256                                                            
         ORG  TRANTAB                                                           
         DC   X'00000000000000000000000000000000'     00 - 0F                   
         DC   X'00000000000000000000000000000000'     10 - 1F                   
         DC   X'00000000000000000000000000000000'     20 - 2F                   
         DC   X'00000000000000000000000000000000'     30 - 3F                   
         DC   X'00000000000000000000000000000000'     40 - 4F                   
         DC   X'00000000000000000000000000000000'     50 - 5F                   
         DC   X'00000000000000000000004000000000'     60 - 6F                   
         DC   X'00000000000000000000000000000000'     70 - 7F                   
         DC   X'00000000000000000000000000000000'     80 - 8F                   
         DC   X'00000000000000000000000000000000'     90 - 9F                   
         DC   X'00000000000000000000000000000000'     A0 - AF                   
         DC   X'00000000000000000000000000000000'     B0 - BF                   
         DC   X'00000000000000000000000000000000'     C0 - CF                   
         DC   X'00000000000000000000000000000000'     D0 - DF                   
         DC   X'00000000000000000000000000000000'     E0 - EF                   
         DC   X'00000000000000000000000000000000'     F0 - FF                   
         ORG                                                                    
*                                                                               
         EJECT                                                                  
*********************************************************************           
* FIELDS USED BY PRINT ROUTINE                                      *           
*********************************************************************           
REPDATE  DS    0CL8                                                             
DATEYR   DS    CL02                                                             
DATEF1   DC    CL01'/'                                                          
DATEJN   DS    CL03                                                             
DATEF2   DC    CL02'  '                                                         
*                                                                               
HDR1     DS    0CL132                       First heading line                  
         DC    CL10' '                                                          
HDR1_DTE DS    CL08                                                             
         DC    CL38' '                                                          
         DC    CL19'SAMPLE QUEUE REPORT'                                        
         DC    CL38' '                                                          
         DC    CL05'PAGE '                                                      
HDR1_PGE DS    CL04                                                             
         DC    CL10' '                                                          
*                                                                               
HDR2     DS    0CL132                       Second heading line                 
         DC    CL25' '                                                          
         DC    CL29'MESSAGE QUEUE MANAGER NAME : '                              
HDR2_QMN DS    CL48                                                             
         DC    CL30' '                                                          
*                                                                               
HDR3     DS    0CL132                       Third heading line                  
         DC    CL41' '                                                          
         DC    CL13'QUEUE NAME : '                                              
HDR3_QUE DS    CL48                                                             
         DC    CL30' '                                                          
*                                                                               
HDR4     DS    0CL132                       Fourth heading line                 
         DC    CL12' '                                                          
         DC    CL08'RELATIVE'                                                   
         DC    CL112' '                                                         
*                                                                               
HDR5     DS    0CL132                       Fifth heading line                  
         DC    CL12' '                                                          
         DC    CL07'MESSAGE'                                                    
         DC    CL113' '                                                         
*                                                                               
HDR6     DS    0CL132                       Sixth heading line                  
         DC    CL11' '                                                          
         DC    CL18' NUMBER   LENGTH '                                          
         DC    CL33'---------------------------------'                          
         DC    CL14' MESSAGE DATA '                                             
         DC    CL33'---------------------------------'                          
         DC    CL23' '                                                          
*                                                                               
REP1     DS    0CL132                       First report line                   
         DC    CL11' '                                                          
REP1_MGN DS    CL08                                                             
         DC    CL01' '                                                          
REP1_MGL DS    CL08                                                             
         DC    CL01' '                                                          
REP1_MSG DS    CL80                                                             
         DC    CL20' '                                                          
*                                                                               
*                                                                               
REP2     DS    0CL132                       second report line                  
         DC    CL11' '                                                          
REP2_MID DS    CL24                                                             
         DC    CL01' '                                                          
REP2_CID DS    CL24                                                             
         DC    CL01' '                                                          
*                                                                               
INF0     DS    0CL132                                                           
         DC    CL48' '                                                          
         DC    CL35'********** END OF REPORT **********'                        
         DC    CL49' '                                                          
*                                                                               
INF1     DS    0CL132                                                           
         DC    CL10' '                                                          
         DC    CL46'********** NO DATA PASSED TO PROGRAM. PROGRAM '             
         DC    CL46'REQUIRES A QUEUE MANAGER NAME AND A QUEUE NAME'             
         DC    CL30'. **********                  '                             
*                                                                               
INF2     DS    0CL132                                                           
         DC    CL25' '                                                          
         DC    CL46'********** NO QUEUE MANAGER NAME PASSED TO PRO'             
         DC    CL25'GRAM - DEFAULT USED *****'                                  
         DC    CL36' '                                                          
*                                                                               
INF3     DS    0CL132                                                           
         DC    CL38' '                                                          
         DC    CL10'**********'                                                 
         DC    CL34' NO QUEUE NAME PASSED TO PROGRAM. '                         
         DC    CL10'**********'                                                 
         DC    CL40' '                                                          
*                                                                               
INF4     DS    0CL132                                                           
         DC    CL13' '                                                          
         DC    CL32'********** AN ERROR OCCURRED IN '                           
INF4_TYP DS    CL10                                                             
         DC    CL20'. COMPLETION CODE = '                                       
INF4_CC  DS    CL04                                                             
         DC    CL16' REASON CODE = '                                            
INF4_RC  DS    CL04                                                             
         DC    CL33' **********                      '                          
*                                                                               
         DS    0D                                                               
CSQBCONN DC    CL8'CSQBCONN'                                                    
*                                                                               
         EJECT                                                                  
         PRINT GEN                                                              
         LTORG                                                                  
VCPRINT  DC    V(CPRINT)                                                        
VPRINT   DC    V(PRINT)                                                         
VPRINTER DC    V(PRINTER)                                                       
VHELLO   DC    V(HELLO)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VHEXIN   DC    V(HEXIN)                                                         
VCARDS   DC    V(CARDS)                                                         
VNUMVAL  DC    V(NUMVAL)                                                        
VTIMBER  DC    V(TIMBER)                                                        
VDATVAL  DC    V(DATVAL)                                                        
VDATCON  DC    V(DATCON)                                                        
VPERVAL  DC    V(PERVAL)                                                        
VGETDAY  DC    V(GETDAY)                                                        
VGETRET  DC    V(GETRET)                                                        
VGETFACT DC    V(GETFACT)                                                       
         EJECT                                                                  
         SPACE 1                                                                
***********************************************************************         
*  MQ API CONSTANTS                                                   *         
***********************************************************************         
         CMQA LIST=NO,EQUONLY=NO                                                
         EJECT                                                                  
* DDDPRINTL                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINTL                                                      
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
*  DSECTS USED BY THIS PROGRAM                                        *         
***********************************************************************         
WORKD    DSECT                                                                  
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
*                                                                               
PARMLIST CALL ,(0,0,0,0,0,0,0,0,0,0,0),VL,MF=L                                  
*                                                                               
PRTREC   DS    0CL133                       Output data record                  
PRTCC    DS    CL1                          Carriage control                    
PRTDATA  DS    CL132                        Data                                
SKIPLINE DS    CL1                          Carriage control store              
*                                                                               
WORKDWRD DS    D                            Used for data conversion            
RELO     DS    A                            Reloaction factor                   
CALLMODE DS    A                            Callers AMODE                       
RBSAVE   DS    F                            Register B save                     
PAGENUM  DS    F                            Page number counter                 
LINENUM  DS    F                            Line number printed                 
MSGNUM   DS    F                            Message count number                
DATALENGTH DS  F                            Actual message length               
OPTIONS  DS    F                            Options                             
COMPCODE DS    F                            Completion code                     
REASON   DS    F                            Reason code                         
HCONN    DS    F                            Connection handle                   
HOBJ     DS    F                                                                
HROBJ    DS    F                                                                
OBJECT   DS    F                            Object handle                       
EXITCODE DS    F                            Exit return code                    
TIMEDATE DS    0CL6                         Used for data conversion            
DATECONV DS    0CL6                         Used for data conversion            
         ORG   DATECONV                                                         
         DS    CL1                                                              
DATE_YR  DS    CL2                          The year                            
DATE_JN  DS    CL3                          The Julian date                     
         ORG                                                                    
PARMADDR DS    F                            Address of parm field               
PARMLEN  DS    H                            Length of parm field                
*                                                                               
MQMNAME  DS    CL48                         Queue manager name                  
MQMQUEUE DS    CL48                         Queue name                          
MQMPMSGL DS    F                            Length of data to put               
AMQMPMSG DS    A                            A(DATA TO PUT)                      
*                                                                               
MSGDATA  DS    CL132                        Used by print routine               
DSPCOMP  DS    CL04                         Display compcode                    
DSPREAS  DS    CL04                         Display reaon                       
DSPMSGL  DS    CL08                         Display message length              
CONVAREA DS    CL8                          Used for data conversion            
*                                                                               
MQMPMSG  DS    CL1000                       DATA TO PUT TO QUEUE                
*                                                                               
WORKX    DS    0D                                                               
*                                                                               
*                                                                               
         DS    0D                                                               
WORKC    CSECT                                                                  
         DS    5000D               WORKING STORAGE                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'068DDTMQPR   05/01/02'                                      
         END                                                                    
