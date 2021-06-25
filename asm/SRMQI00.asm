*          DATA SET SRMQI00    AT LEVEL 023 AS OF 01/02/01                      
*PHASE T17000                                                                   
*                                                                               
***********************************************************************         
* MQIO - FACPAK ONLINE MQ SERIES UTILITY CALLED BY FATASKER           *         
*        INTERFACE TO MQ SERIES SHEDULER/SUBMITTER SUBTASK FAMQIO     *         
*                                                                     *         
* PARAMETER LIST SEE DDMQIOPD                                         *         
*                                                                     *         
* COMMANDS USED IN THIS PROGRAM:                                      *         
*                                                                     *         
* 'RESET   ': FACPAK CONTROL COMMAND (FROM TASKER) TO RESET STATE     *         
*             OF FACPAK INPUT MQ QUEUES.                              *         
*                                                                     *         
* 'INPUT   ': FACPAK CONTROL COMMAND (FROM TASKER) TO GET DATA INTO   *         
*             TASK AREA AT START OF TRANSACTION FROM FACPAK INPUT     *         
*             MQ QUEUES                                               *         
*                                                                     *         
* 'END     ': CLEAR OR END CURRENT MQ REQUESTS FOR FACPAK TASKS       *         
*             AFTER MQIO ERROR EVENT                                  *         
*                                                                     *         
***********************************************************************         
         TITLE 'SRMQI00 - MQ DARE MESSAGE ONLY CALLED BY FATASKER'              
MQIO     CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKX-WRKD,**$MQI**,RA,CLEAR=YES,RR=RE                            
         USING WRKD,RC             RC=A(W/S)                                    
         ST    RE,RELO                                                          
*                                                                               
         USING SRPARMD,R1          R1=A(S/R PARAM LIST)                         
         L     RF,SRQASYSF         INTIALISE FACPAK ADDRESSES                   
         ST    RF,ASYSFAC                                                       
         DROP  R1                                                               
*                                                                               
         L     RE,VMQIPARM-SYSFACD(RF)                                          
         ST    RE,APARM            A(MQIO PARMS)                                
         MVC   PARM,0(RE)                                                       
         MVC   AMQIOLST,L'PARM(RE)    A(MQIOLST) FROM FATASKER                  
         MVC   AMQIOECB,L'PARM+4(RE)  A(MQIOECB) FROM FATASKER                  
         MVC   AMQLINIT,L'PARM+8(RE)  A(MQLINIT) FROM FATASKER                  
         MVC   AATCFLAG,L'PARM+12(RE) A(ATCFLAG) FROM FATASKER                  
*                                                                               
         L     RE,VSSB-SYSFACD(RF)                                              
         ST    RE,ASSB                                                          
         L     RE,VWCTYPE-SYSFACD(RF)                                           
         ST    RE,AWCTYPE                                                       
         L     RE,VDMOD000-SYSFACD(RF)                                          
         ST    RE,ADMOD000                                                      
         L     RE,VTCB-SYSFACD(RF)                                              
         ST    RE,ATCB                                                          
         L     RE,VUTL-SYSFACD(RF)                                              
         ST    RE,AUTL                                                          
         MVC   ALCM,VLCM-SYSFACD(RF)                                            
         L     RF,=V(MSGQIN)                                                    
         ST    RF,AMSGQIN                                                       
         L     RE,ASSB                                                          
         MVC   SVMQION,SSBMQION-SSBD(RE)                                        
         L     RE,SSBTKADR-SSBD(RE)                                             
         ST    RE,ATCBTASK                                                      
         L     RF,TCBUTL-TCBD(RE)                                               
         ST    RF,AUTLTASK                                                      
         L     RF,TCBTWA-TCBD(RE)                                               
*                                                                               
         B     PROCCMD                                                          
*                                                                               
EXIT     XMOD1                                                                  
         EJECT                                                                  
*                                                                               
         USING MQIOD,R6            R6=A(MQIO LOCAL CONTROL BLOCK)               
***********************************************************************         
* PROCESS COMMAND TO MQIO FROM CALLER                                 *         
***********************************************************************         
PROCCMD  EQU   *                   PROCESS CALLER COMMAND                       
         SR    RF,RF                                                            
         ICM   RF,7,PMQCMD         GET COMMAND FROM INPUT PARAMETER             
         XC    PMQRET,PMQRET       CLEAR RETURN STATUS CODE                     
         CLC   =CL8'RESET',0(RF)                                                
         BE    PROCSET                                                          
         CLC   =CL8'INPUT',0(RF)                                                
         BE    PROCINP                                                          
         CLC   =CL8'END',0(RF)                                                  
         BE    PROCEND                                                          
         DC    H'0'                UNKNOWN COMMAND                              
         EJECT                                                                  
***********************************************************************         
* PROCESS CONTROL COMMAND 'RESET' FROM FACPAK TASKER TO RESET STATE   *         
*         OF FACPAK INPUT MQ QUEUES                                   *         
***********************************************************************         
PROCSET  EQU   *                                                                
         L     RE,AMQLINIT                                                      
         CLI   0(RE),0             TEST INITLIASED STATUS FLAG                  
         BE    PSET010                                                          
         BAS   RE,INITMQL          CALL MQIO INITIALISATION ROUTINE             
         L     RE,AMQLINIT                                                      
         MVI   0(RE),0             SET INITIALISATION STATUS FLAG               
*                                                                               
*                                  PROCESS MQIO CONTROL PARAMETER LIST          
*                                  FOR INPUT QUEUES AWAITING RESET              
PSET010  EQU   *                                                                
         L     R4,AMQIOLST         R4=A(MQIO CONTROL PARAMETER LIST)            
         USING MQIOLD,R4                                                        
PSET020  TM    MQIOLFLG,MQIOLCTL   TEST FACPAK CONTROL QUEUE                    
         BO    PSETCT                                                           
         TM    MQIOLFLG,MQIOLRD    TEST FACPAK INPUT READ QUEUE                 
         BO    PSETRD                                                           
PSETNXT  ICM   R4,15,MQIOLNXT      GET NEXT ENTRY IN MQIOLST                    
         BZ    EXIT                                                             
         B     PSET020                                                          
*                                                                               
PSETCT   EQU   *                   FACPAK CONTROL Q RESET                       
         TM    MQIOLFLG,MQIOLBSY   IGNORE IF CURRENTLY BUSY                     
         BO    PSETNXT                                                          
         BAS   RE,GETATC           GET A(FAATC ENTRY)                           
         BNE   EXIT                                                             
         BAS   RE,SETCT            RESET FACPAK CONTROL QUEUE                   
         BNE   EXIT                                                             
         B     PSETNXT                                                          
*                                                                               
PSETRD   EQU   *                   FACPAK INPUT Q RESET                         
         BAS   RE,GETATC           GET A(FAATC ENTRY)                           
         BNE   EXIT                                                             
         BAS   RE,SETRD            RESET FACPKA INPUT QUEUE                     
         BNE   EXIT                                                             
         B     PSETNXT                                                          
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS CONTROL COMMAND 'INPUT' FROM FACPAK TASKER TO GET DATA      *         
*         INTO TASK AREA AT START OF TRANSACTION FROM FACPAK INPUT    *         
*         MQ QUEUES                                                   *         
***********************************************************************         
         SPACE 1                                                                
PROCINP  EQU   *                   PROCESS Q DATA RETURNED FROM FAMQIO          
         L     RE,AMQIOECB                                                      
         XC    0(4,RE),0(RE)                                                    
*                                  PROCESS MQIO CONTROL PARAMETER LIST          
*                                  FOR INPUT QUEUES DATA                        
         L     R4,AMQIOLST                                                      
         USING MQIOLD,R4                                                        
PINP010  EQU   *                                                                
         TM    MQIOLECB,X'40'      TEST INPUT ECB FLAGGED BY FAMQIO             
         BZ    PINPNXT                                                          
         XC    MQIOLECB,MQIOLECB   CLEAR ECB FOR NEXT TIME                      
         TM    MQIOLFLG,MQIOLCTL   TEST FACPAK CONTROL QUEUE                    
         BO    PINPCT                                                           
         TM    MQIOLFLG,MQIOLRD    TEST FACPAK INPUT READ QUEUE                 
         BO    PINPRD                                                           
PINPNXT  ICM   R4,15,MQIOLNXT      GET NEXT ENTRY IN MQIOLST                    
         BZ    EXIT                                                             
         B     PINP010                                                          
*                                                                               
PINPCT   EQU   *                   FACPAK CONTROL Q DATA FROM FAMQIO            
         BAS   RE,INPCT            GET INPUT DATA FROM CONTROL QUEUE            
         BNE   EXIT                                                             
         NI    MQIOLFLG,X'FF'-MQIOLBSY                                          
         B     PINPNXT                                                          
*                                                                               
PINPRD   EQU   *                   FACPAK INPUT Q DATA FROM FAMQIO              
         BAS   RE,INPRD            GET INPUT DATA FROM INPUT QUEUE              
         BNE   EXIT                                                             
         B     PINPNXT                                                          
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS COMMAND 'END' TO END OR CLEAR CURRENT OUTSTANDING MQ        *         
*         REQUESTS FACPAK TASKS AFTER MQIO ERROR EVENT                *         
***********************************************************************         
         SPACE 1                                                                
PROCEND  EQU   *                                                                
*                                                                               
         CLC   SVMQION,=F'2'                                                    
         BNE   WTOLAB9                                                          
         ICM   RF,15,=C'LVL1'      OUTPUT INFO MESSAGE TO CONSOLE               
         GOTO1 ADMOD000,DMCB,AWCTYPE,MSG01,L'MSG01,(RF)                         
WTOLAB9  EQU   *                                                                
*                                                                               
         L     R3,ATCB             SEARCH TCB FOR MQIO REQUESTS PENDING         
         LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING TCBD,R3                                                          
*                                                                               
PEND010  TM    TCBFLAG3,TCBMQWT                                                 
         BZ    PEND020                                                          
         ICM   R1,15,TCBMQERT                                                   
         BZ    PEND020                                                          
         LA    R2,999                                                           
         POST  (1),(2)                                                          
*                                                                               
PEND020  BXLE  R3,R4,PEND010                                                    
         DROP  R3                                                               
*                                                                               
         L     RE,AMQIOECB                                                      
         XC    0(4,RE),0(RE)                                                    
*                                  SEARCH MQIO CONTROL PARAMETER LIST           
*                                  FOR PENDING EVENTS                           
         L     R4,AMQIOLST                                                      
         USING MQIOLD,R4                                                        
PEND100  EQU   *                                                                
         TM    MQIOLFLG,MQIOLRD                                                 
         BO    PEND120                                                          
PEND110  ICM   R4,15,MQIOLNXT                                                   
         BZ    PEND200                                                          
         B     PEND100                                                          
*                                                                               
PEND120  EQU   *                                                                
         ICM   R6,15,MQIOLC                                                     
         USING UTLD,R7                                                          
         ICM   R7,15,MQIOLUTL                                                   
         BZ    PEND110                                                          
         NI    TSTAT2,X'FF'-TSTATTIP                                            
         B     PEND110                                                          
*                                                                               
PEND200  EQU   *                                                                
         B     EXIT                                                             
         DROP  R4,R7                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET FACPAK INPUT QUEUE READY FOR NEXT INCOMING MESSAGE   *         
***********************************************************************         
         USING MQIOLD,R4                                                        
SETRD    NTR1                                                                   
         USING UTLD,R7                                                          
         ICM   R7,15,MQIOLUTL                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    TSTAT2,TSTATTIP                                                  
         BO    SERDOK                                                           
*                                                                               
         TM    MQIOLFLG,MQIOLBSY                                                
         BZ    SERD010                                                          
*                                                                               
         TM    MQIOLFLG,MQIOLWTG                                                
         BZ    SERDOK                                                           
         NI    MQIOLFLG,X'FF'-(MQIOLWTG+MQIOLBSY)                               
*                                                                               
SERD010  EQU   *                                                                
         ICM   R3,15,ATCENTRY      GET A(ATC ENTRY)                             
         USING ATCD,R3             R3=A(ATC)                                    
*                                                                               
         LA    R1,ATCECB                                                        
         POST  (1)                                                              
*                                                                               
         B     SERDOK                                                           
*                                                                               
SERDOK   SR    RC,RC                                                            
SERDNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET FACPAK CONTROL QUEUE READY FOR NEXT INCOMING MESSAGE *         
***********************************************************************         
         SPACE 1                                                                
         USING MQIOLD,R4                                                        
SETCT    NTR1                                                                   
         ICM   R3,15,ATCENTRY      GET A(ATC ENTRY)                             
         USING ATCD,R3             R3=A(ATC)                                    
*                                                                               
         LA    R1,ATCECB                                                        
         POST  (1)                                                              
*                                                                               
         B     SECTOK                                                           
*                                                                               
SECTOK   SR    RC,RC                                                            
SECTNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET INPUT DATA FROM FACPAK INPUT READ QUEUE AND SCHEDULE *         
* FACPAK TRANSACTION TO PROCESS THAT DATA                             *         
***********************************************************************         
         SPACE 1                                                                
         USING MQIOLD,R4                                                        
INPRD    NTR1                                                                   
         ICM   R6,15,MQIOLC        R6=A(MQIO CONTROL BLOCK)                     
*                                                                               
         USING UTLD,R7                                                          
         ICM   R7,15,MQIOLUTL      R7=A(DUMMY MQ UTL)                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    TSTAT2,TSTATTIP     SET UTL ENTRY 'IN PROGRESS'                  
         MVI   TSYS,0              SET UP SRMQP00                               
         MVI   TSVCREQ,X'01'                                                    
         MVI   TSVCREQ+1,X'6B'                                                  
         XC    TSVDATA1,TSVDATA1   CLEAR THIS FOR SAFETY                        
*                                                                               
         ICM   R3,15,TBUFF         BUILD MESSAGE INTO UTL BUFFER                
         SH    R3,=H'8'                                                         
         USING TBHD,R3                                                          
         STCM  R7,15,TBHAUTL                                                    
         MVI   TBHINDS,0                                                        
         ICM   RF,15,MQIOLDAT                                                   
         LA    RE,MSGDESC_LENGTH                                                
         AR    RF,RE                                                            
         STCM  RF,3,TBHMSGL                                                     
*                                                                               
         ICM   R0,15,TBUFF                                                      
         LA    RE,MQIOMSGD                                                      
         LA    R1,MSGDESC_LENGTH                                                
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                  QUEUE SRMQP00 THRU MSGQIN                    
         GOTO1 AMSGQIN,DMCB,ASYSFAC,TBUFF,(R7)                                  
*                                                                               
         OI    MQIOLFLG,MQIOLWTG   FLAG THIS INPUT QUEUE WAITING                
*                                                                               
         CLC   SVMQION,=F'2'                                                    
         BNE   WTOLABD                                                          
         ICM   RF,15,=C'LVL1'      OUTPUT INFO MESSAGE TO CONSOLE               
         GOTO1 ADMOD000,DMCB,AWCTYPE,MSG02,L'MSG02,(RF)                         
WTOLABD  EQU   *                                                                
*                                                                               
         B     INRDOK                                                           
*                                                                               
INRDOK   SR    RC,RC                                                            
INRDNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R3,R4,R7                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET INPUT DATA FROM FACPAK CONTROL QUEUE AND CALL THE    *         
* ROUTINE TO PROCESS THAT DATA                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING MQIOLD,R4                                                        
INPCT    NTR1                                                                   
         ICM   R6,15,MQIOLC                                                     
*                                                                               
         CLC   SVMQION,=F'2'                                                    
         BNE   INCT010                                                          
         LA    RF,MSG03                                                         
         ICM   RE,15,MQIOABUF                                                   
         MVC   34(46,RF),0(RE)                                                  
         ICM   RF,15,=C'LVL1'      OUTPUT INFO MESSAGE TO CONSOLE               
         GOTO1 ADMOD000,DMCB,AWCTYPE,MSG03,L'MSG03,(RF)                         
INCT010  EQU   *                                                                
*                                                                               
*                                  FIND ENTRY IN CQIMTAB                        
         ICM   R2,15,MQIOABUF                                                   
         LA    R5,CQIMTAB                                                       
INCT100  EQU   *                                                                
         CLI   0(R5),X'FF'                                                      
         BE    INCT200                                                          
         CLC   4(8,R5),0(R2)                                                    
         BE    INCT110                                                          
         LA    R5,12(R5)                                                        
         B     INCT100                                                          
*                                                                               
INCT110  EQU   *                                                                
         ICM   RF,15,0(R5)                                                      
         ICM   R3,15,MQIOLBUF                                                   
         GOTO1 (RF),DMCB,(R2),(R3),RR=RELO                                      
         B     INCTOK                                                           
*                                                                               
INCT200  EQU   *                                                                
         CLC   SVMQION,=F'2'                                                    
         BNE   WTOLABE                                                          
         ICM   RF,15,=C'LVL1'      OUTPUT INFO MESSAGE TO CONSOLE               
         GOTO1 ADMOD000,DMCB,AWCTYPE,MSG04,L'MSG04,(RF)                         
WTOLABE  EQU   *                                                                
         B     INCTOK                                                           
*                                                                               
INCTOK   SR    RC,RC                                                            
INCTNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
GETATC   NTR1                      LOCATE A FREE FAMQIO SCHEDULER               
         L     RE,ASSB                                                          
         L     R3,SSBAATC-SSBD(RE)                                              
         LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING ATCD,R3             R3=A(ATC)                                    
*                                                                               
GATC010  CLI   ATCTYPE,ATCTMQIO    TEST MQIO SUB TASK                           
         BNE   GATC020                                                          
         TM    ATCSTAT1,ATCSATCH   TEST TASK IS ATTACHED                        
         BZ    GATC020                                                          
         TM    ATCSTAT1,ATCSBUSY   TEST TASK IS BUSY                            
         BNZ   GATC020                                                          
         OC    ATCATCB,ATCATCB     TEST ALLOCATED TO ANOTHER TASK               
         BNZ   GATC020                                                          
         TM    ATCSTAT1,ATCSERRS   TEST TASK IS IN ERROR                        
         BNZ   GATC020                                                          
         B     GATC040                                                          
*                                                                               
GATC020  BXLE  R3,R4,GATC010                                                    
         L     RE,AATCFLAG                                                      
         CLI   0(RE),C'N'                                                       
         BE    GATCNO                                                           
         CLC   SVMQION,=F'2'                                                    
         BNE   WTOLABF                                                          
         ICM   RF,15,=C'LVL1'      OUTPUT INFO MESSAGE TO CONSOLE               
         GOTO1 ADMOD000,DMCB,AWCTYPE,MSG05,L'MSG05,(RF)                         
WTOLABF  EQU   *                                                                
         L     RE,AATCFLAG                                                      
         MVI   0(RE),C'N'                                                       
         B     GATCNO                                                           
*                                                                               
GATC040  EQU   *                                                                
         STCM  R3,15,ATCENTRY                                                   
         L     RE,AATCFLAG                                                      
         CLI   0(RE),C'Y'                                                       
         BE    GATCOK                                                           
         CLC   SVMQION,=F'2'                                                    
         BNE   WTOLABG                                                          
         ICM   RF,15,=C'LVL1'      OUTPUT INFO MESSAGE TO CONSOLE               
         GOTO1 ADMOD000,DMCB,AWCTYPE,MSG06,L'MSG06,(RF)                         
WTOLABG  EQU   *                                                                
         L     RE,AATCFLAG                                                      
         MVI   0(RE),C'Y'                                                       
         B     GATCOK                                                           
*                                                                               
GATCOK   SR    RC,RC                                                            
GATCNO   LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
INITMQL  NTR1                      INITIALISE MQIOLST                           
         USING MQIOLD,R4                                                        
         L     R4,AMQIOLST                                                      
IMQL010  TM    MQIOLFLG,MQIOLRD                                                 
         BO    IMQL100                                                          
IMQL020  ICM   R4,15,MQIOLNXT                                                   
         BZ    IMQLOK                                                           
         B     IMQL010                                                          
*                                                                               
         USING UTLD,R7                                                          
IMQL100  EQU   *                                                                
         GOTO1 ALCM,DMCB,4                                                      
         LTR   R3,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 ALCM,DMCB,VTGETUTL                                               
         LTR   R7,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R7,MQIOLUTL                                                      
         ST    R3,TBUFF                                                         
         OI    TSTAT1,TSTATDDS     SET DDS/3270/DUMMY TERMINAL                  
         OI    TTYPE2,TTYPEMQ                                                   
         OI    TTYPE2,TTYPEDUM                                                  
         OI    TFLAG,TFLAGIRB      SET INHIBIT RECEIVING BROADCASTS             
         MVC   TSYM,=C'DUMMYMQT'                                                
*                                                                               
         L     RE,ASSB                                                          
         USING SSBD,RE                                                          
         LH    RF,SSBDTRMS                                                      
         LA    RF,1(RF)                                                         
         STH   RF,SSBDTRMS                                                      
         LH    RF,SSBTRMS                                                       
         LA    RF,1(RF)                                                         
         STH   RF,SSBTRMS                                                       
         DROP  RE                                                               
*                                                                               
         ICM   R6,15,MQIOLC                                                     
*                                                                               
         LA    RF,MSGDESC_LENGTH                                                
         AR    R3,RF                                                            
         STCM  R3,15,MQIOABUF                                                   
         LA    RE,2024                                                          
         SR    RE,RF                                                            
         STCM  RE,15,MQIOLBUF                                                   
*                                                                               
         CLC   SVMQION,=F'2'                                                    
         BNE   WTOLABH                                                          
         ICM   RF,15,=C'LVL1'      OUTPUT INFO MESSAGE TO CONSOLE               
         GOTO1 ADMOD000,DMCB,AWCTYPE,MSG06,L'MSG06,(RF)                         
WTOLABH  EQU   *                                                                
*                                                                               
         B     IMQL020                                                          
*                                                                               
IMQLOK   SR    RC,RC                                                            
IMQLNO   LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO PROCESS CONTROL Q INPUT DATA FOR 'DAREMSG1'             *          
*                                                                    *          
* PARAMETERS:                                                        *          
* P1 = A(DATA BUFFER)                                                *          
* P2 = LENGTH DATA BUFFER                                            *          
**********************************************************************          
         SPACE 1                                                                
INDAREM1 NTR1                    ROUTINE TO PROCESS CONTROL Q DATA              
         LM    R2,R3,0(R1)       R2=A(DATA BUFFER), R3=LENGTH DATA              
*                                                                               
         CLC   SVMQION,=F'2'     WRITE OUT MESSAGE AND DATA TO JESMSG           
         BNE   IDM120                                                           
         LR    RE,R3                                                            
         CLM   RE,15,=AL4(30)                                                   
         BH    *+8                                                              
         LA    RE,30                                                            
         LA    RF,MSG07                                                         
         MVC   34(30,RF),SPACES                                                 
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   34(0,RF),0(R2)                                                   
         ICM   RF,15,=C'LVL1'      OUTPUT INFO MESSAGE TO CONSOLE               
         GOTO1 ADMOD000,DMCB,AWCTYPE,MSG07,L'MSG07,(RF)                         
*                                                                               
* INCOMING MESSAGE FORMAT IS AS FOLLOWS:                                        
* CL08 'DAREMSG1'                                                               
* CL05 UTL NUMBER                                                               
* CL03 USER INITIALS                                                            
* CL04 TIME HHMM                                                                
* CL01 MAIL TYPE                                                                
*                                                                               
NEW      USING TBDDARD,WORK                                                     
IDM120   LA    R2,8(R2)            POINT TO MESSAGE                             
*                                                                               
         XC    WORK,WORK                                                        
         CLC   =C'***',0(R2)       SPECIAL FOR ADV SYSTEM                       
         BNE   IDM130              NEXT 2 CHARACTERS ARE THE AGENCY             
         MVC   NEW.TBDAGY,3(R2)    CODE IN EBCDIC                               
         B     IDM140                                                           
*                                                                               
IDM130   DS    0H                                                               
         PACK  DUB,0(5,R2)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,NEW.TBDUSER    SET USER-ID                                  
*                                                                               
IDM140   DS    0H                                                               
         MVC   NEW.TBDINIT,5(R2)   COPY IN INITIALS OR SET ZERO                 
         CLC   NEW.TBDINIT,=CL3' '                                              
         BH    *+10                                                             
         XC    NEW.TBDINIT,NEW.TBDINIT                                          
*                                                                               
         PACK  DUB,8(4,R2)         NEED TIME HHMM AS PWOS PL2                   
         SRP   DUB,11,0                                                         
         MVC   NEW.TBDTIME,DUB                                                  
*                                                                               
         MVC   NEW.TBDTYPE,12(R2)  SET MAIL TYPE                                
*                                                                               
         STAR  CLEAR=YES,ARS=ON    SAVE CURRENT ACCESS REGISTER STATUS          
*                                                                               
         L     R2,=A(DTDARE)                                                    
         N     R2,=X'000000FF'     GET DISPLACEMENT TO DARE TABLE               
         SLL   R2,6                                                             
         L     RE,ASSB                                                          
         LAM   R2,R2,SSBTBLET-SSBD(RE)                                          
         ICM   R2,15,DSPTFRST-DMSPACED(R2)                                      
         USING TABDARED,R2                                                      
*                                                                               
IDM150   CLC   TBDMAX,TBDNOW       FREE SLOTS?                                  
         BNH   IDM160              NO - JUST EXIT + IGNORE POSTING              
*                                                                               
         ICM   RE,15,TBDNOW        FIND CURRENT HIGH WATER                      
         LA    RF,1(RE)                                                         
         CS    RE,RF,TBDNOW                                                     
         BNE   IDM150                                                           
*                                                                               
         LA    RF,TBDDARL          INDEX INTO UNSORTED ARRAY                    
         MSR   RE,RF                                                            
         ICM   R2,15,TBDUNSRT                                                   
         AR    R2,RE               R2 POINTS TO THIS ENTRY                      
*                                                                               
         MVC   0(TBDDARL,R2),NEW.TBDDARD                                        
*                                                                               
IDM160   REAR  ARS=OFF                                                          
         B     IDM1OK                                                           
*                                                                               
IDM1OK   SR    RC,RC                                                            
IDM1NO   LTR   RC,RC                                                            
         XIT1                                                                   
*                                                                               
         DROP  NEW,R2                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         LTORG                                                                  
*                                                                               
MSG01    DC    C'*FACPAX* DDMQIO END CALLED'                                    
MSG02    DC    C'*FACPAX* DDMQIO AFTER INTRD'                                   
MSG03    DC    CL80'*FACPAX* DDMQIO DATA AFTER INTCT: '                         
MSG04    DC    C'*FACPAX* DDMQIO UNDEFINED CQ INPUT'                            
MSG05    DC    C'*FACPAX* DDMQIO ATC NOT FOUND'                                 
MSG06    DC    C'*FACPAX* DDMQIO ATC FOUND'                                     
MSG07    DC    CL80'*FACPAX* DDMQIO CQI DAREMSG1:'                              
SPACES   DC    CL256' '                                                         
         EJECT                                                                  
***********************************************************************         
* MQIO SHCEDULER CONTROL Q INPUT MESSAGE TABLE                        *         
***********************************************************************         
CQIMTAB  DS    0D                                                               
         DC    AL4(INDAREM1)                                                    
         DC    CL8'DAREMSG1'                                                    
*                                                                               
CQIMTABX DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*  MQ API CONSTANTS                                                   *         
***********************************************************************         
         CMQA LIST=NO                                                           
*                                                                               
OBJDESC    CMQODA  DSECT=NO,LIST=YES        OBJECT DESCRIPTOR                   
MSGDESC    CMQMDA  DSECT=NO,LIST=YES        MESSAGE DESCRIPTOR                  
PUTMSGOPTS CMQPMOA DSECT=NO,LIST=YES        PUT MESSAGE OPTIONS                 
GETMSGOPTS CMQGMOA DSECT=NO,LIST=YES        GET MESSAGE OPTIONS                 
*                                                                               
         EJECT                                                                  
* DDMQBMC                                                                       
       ++INCLUDE DDMQBMC                                                        
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER TEMP W/S                                             *         
***********************************************************************         
WRKD     DSECT                                                                  
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PLIST    DS    8F                                                               
RELO     DS    A                                                                
APARM    DS    A                   A(PARAMETER LIST)                            
*                                                                               
PARM     DS    0XL24               PARAMETER LIST                               
PMQRET   DS    AL1                 RETURN CODE                                  
PMQCMD   DS    AL3                 A(MQIO COMMAND)                              
PMQDATA  DS    AL4                 A(DATA)                                      
PMQDLEN  DS    AL4                 DATA LENGTH                                  
PMQAQNM  DS    AL4                 A(MQ QUEUE NAME)                             
PMQAIOB  DS    AL4                 A(IO BUFFER)                                 
PMQAERB  DS    AL4                 A(ERROR INFO BLOCK)                          
*                                                                               
AMQIOLST DS    A                                                                
AMQIOECB DS    A                                                                
AMQLINIT DS    A                                                                
AATCFLAG DS    A                                                                
*                                                                               
AMQIT    DS    A                                                                
ATCENTRY DS    A                                                                
MQRETECB DS    F                                                                
*                                                                               
ASYSFAC  DS    A                                                                
AMSGQIN  DS    A                                                                
AWCTYPE  DS    A                                                                
ADMOD000 DS    A                                                                
ASSB     DS    A                                                                
ATCB     DS    A                                                                
ATCBTASK DS    A                                                                
AUTL     DS    A                                                                
AUTLTASK DS    A                                                                
ALCM     DS    A                                                                
ATWA     DS    A                                                                
ATIA     DS    A                                                                
ACOMFACS DS    A                                                                
SVMQION  DS    F                                                                
*                                                                               
WORK     DS    CL255                                                            
MQIOC    DS    XL(MQIODLQ)                                                      
*                                                                               
WRKX     EQU   *                   END OF TOTAL W/S                             
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
* FATBHD                                                                        
       ++INCLUDE FATBHD                                                         
         EJECT                                                                  
* FATABSDEQU                                                                    
       ++INCLUDE FATABSDEQU                                                     
         EJECT                                                                  
* FATABSDAR                                                                     
       ++INCLUDE FATABSDAR                                                      
         EJECT                                                                  
* DDMQIOD                                                                       
       ++INCLUDE DDMQIOD                                                        
* DDMQIOPD                                                                      
       ++INCLUDE DDMQIOPD                                                       
         EJECT                                                                  
* DMSPACED                                                                      
       ++INCLUDE DMSPACED                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023SRMQI00   01/02/01'                                      
         END                                                                    
