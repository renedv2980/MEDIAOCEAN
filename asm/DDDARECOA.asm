*          DATA SET DDDARECOA  AT LEVEL 191 AS OF 02/28/19                      
*PHASE DARECOAA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
***********************************************************************         
*  TITLE:        DARE COA PROCESSOR                                   *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- WORK                                           *         
*                R3 -- WORK                                           *         
*                R4 -- NOT USED                                       *         
*                R5 -- NOT USED                                       *         
*                R6 -- NOT USED                                       *         
*                R7 -- NOT USED                                       *         
*                R8 -- NOT USED                                       *         
*                R9 -- 2ND BASE FOR COMMON STORAGE AREA               *         
*                RA -- CPRINT                                         *         
*                RB -- PROGRAM BASE                                   *         
*                RC -- 1ST BASE FOR COMMON STORAGE AREA               *         
*                RD -- REGISTER SAVE AREA                             *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'DARE COA PROCESSOR'                                             
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
DARECOA  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*DARCOA*,=A(R13CHAIN)                                          
*                                                                               
         ENTRY UTL                 FOR DATAMGR                                  
         ENTRY SSB                                                              
*                                                                               
         L     RC,=A(COMMWORK)     COMMON STORAGE AREA                          
         LR    R9,RC                                                            
         AHI   R9,4096                                                          
         USING COMMWORK,RC,R9                                                   
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         BRAS  RE,INITIAL          INITIALIZE                                   
*                                                                               
NEXTMSG  BRAS  RE,WAIT             WAIT FOR INCOMING MESSAGE                    
         BNE   GOODBYE             SHUTDOWN BY OPERATOR                         
*                                                                               
*                                  JUST GOT ONE COA MESSAGE                     
         BRAS  RE,VALCOA           VALIDATE COA MESSAGE                         
         BNE   COMMIT                  NOT VALID - TRY NEXT MESSAGE             
         BRAS  RE,GET1DNT          NOW, GET DELNOT WITH THIS CORRELID           
         BNE   COMMIT                  NOT FOUND - TRY NEXT MESSAGE             
         BRAS  RE,PUT1WRKQ         AND PUT THE DELNOT TO WORKQ                  
         B     COMMIT                                                           
*                                                                               
COMMIT   LAY   R2,CSQBCOMM_STRUCTURE                                            
         BRAS  RE,CALLMQ           COMMIT COA/DELNOT/WORKQ                      
         CLC   COMPCODE,=A(MQCC_OK)                                             
         BE    *+6                                                              
         DC    H'0'                NO ERROR IS ACCEPTABLE                       
         B     NEXTMSG             TRY TO GET NEXT MESSAGE                      
*                                                                               
GOODBYE  DS    0H                                                               
         LAY   R2,CSQBCOMM_STRUCTURE                                            
         BRAS  RE,CALLMQ                                                        
         CLC   COMPCODE,=A(MQCC_OK)                                             
         BE    *+6                                                              
         DC    H'0'                NO ERROR IS ACCEPTABLE                       
*                                                                               
         LAY   R2,CSQBCLOS_STRUCTURE_COA                                        
         BRAS  RE,CALLMQ                                                        
         LAY   R2,CSQBCLOS_STRUCTURE_DELNOT                                     
         BRAS  RE,CALLMQ                                                        
*                                                                               
         CLC   QNAME_LOG,SPACES                                                 
         BE    MQLOGB                                                           
         LAY   R2,CSQBCLOS_STRUCTURE_LOG                                        
         BRAS  RE,CALLMQ                                                        
*                                                                               
         CLC   QNAME_LOG2,SPACES                                                
         BE    MQLOGB                                                           
         LAY   R2,CSQBCLOS_STRUCTURE_LOG2                                       
         BRAS  RE,CALLMQ                                                        
*                                                                               
MQLOGB   CLC   QNAME_LOGB,SPACES   BLOCKCHAIN QUEUE                             
         BE    MQDISC                                                           
         LAY   R2,CSQBCLOS_STRUCTURE_LOGB                                       
         BRAS  RE,CALLMQ                                                        
*                                                                               
MQDISC   LAY   R2,CSQBDISC_STRUCTURE                                            
         BRAS  RE,CALLMQ           DISCONNECT FROM MQ MANAGER                   
*                                                                               
         PRNT  EXITING,PRINT=ALWAYS                                             
*                                                                               
         XBASE                                                                  
                                                                                
***********************************************************************         
* INITIALIZE                                                          *         
***********************************************************************         
INITIAL  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,READCARD         READ CARDS                                   
         BRAS  RE,SETMQ            INT MQ ROUTINES                              
         BRAS  RE,SETOPS           INT OPS LISTENER                             
         XIT1  ,                                                                
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
***********************************************************************         
* READ CARDS                                                          *         
*                                                                               
* DSPACE=CL1         DSPACE CONTROL CARD                                        
* MQQMGRNAME=CL48    QUEUE MANAGER NAME                                         
* MQCOAQ=CL48        COA QUEUE NAME                                             
* MQLOGQ=CL48        LOG QUEUE NAME                                             
* MQLOGQ2=CL48       LOG QUEUE NAME                                             
* MQDELNOTQ=CL48     DELNOT QUEUE NAME                                          
* TRACE=CL1          Y/N PRINT OUT                                              
* WAIT_DELNOT=CL1    NUMBER OF SECONDS (DEFAULT IS 2 SECONDS)                   
***********************************************************************         
READCARD NTR1  BASE=*,LABEL=*                                                   
*                                                                               
RC20     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BE    RCX                                                              
*                                                                               
         CLI   CARD,C'*'           COMMENT CARD?                                
         BE    RC20                YES                                          
*                                                                               
         MVC   P(L'CARD),CARD                                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLC   =C'DSPACE=',CARD    DSPACE=                                      
         BNE   RC30                                                             
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(1,RF),CARD+7                                     
         B     RC20                                                             
*                                                                               
RC30     CLC   =C'WAIT_DELNOT=',CARD    WAIT_DELNOT=  ONLY 1 DIGIT              
         BNE   RC40                                                             
         NI    CARD+11,X'0F'            ONLY KEEP 2ND NIBBLE                    
         SR    R1,R1                                                            
         IC    R1,CARD+11               R1=# SECONDS                            
         MHI   R1,1000                  X 1000 (# MILISEC)                      
         ST    R1,GDNTWAIT                                                      
         B     RC20                                                             
*                                                                               
RC40     CLC   =C'MQQMGRNAME=',CARD                                             
         BNE   *+14                                                             
         MVC   QMGRNAME,CARD+11                                                 
         B     RC20                                                             
*                                                                               
         CLC   =C'MQCOAQ=',CARD                                                 
         BNE   *+14                                                             
         MVC   QNAME_COA,CARD+7                                                 
         B     RC20                                                             
*                                                                               
         CLC   =C'MQLOGQ=',CARD                                                 
         BNE   *+14                                                             
         MVC   QNAME_LOG,CARD+7                                                 
         B     RC20                                                             
*                                                                               
         CLC   =C'MQLOGQ2=',CARD                                                
         BNE   *+14                                                             
         MVC   QNAME_LOG2,CARD+8                                                
         B     RC20                                                             
*                                                                               
         CLC   =C'MQLOGQB=',CARD                                                
         BNE   *+14                                                             
         MVC   QNAME_LOGB,CARD+8                                                
         B     RC20                                                             
*                                                                               
         CLC   =C'MQDELNOTQ=',CARD                                              
         BNE   *+14                                                             
         MVC   QNAME_DELNOT,CARD+10                                             
         B     RC20                                                             
*                                                                               
         CLC   =C'TRACE=',CARD                                                  
         BNE   *+14                                                             
         MVC   TRACEFLG,CARD+6                                                  
         B     RC20                                                             
*                                                                               
RCX      XIT1  ,                                                                
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
***********************************************************************         
* INITIALIZE MQ ROUTINES                                              *         
***********************************************************************         
SETMQ    NTR1  BASE=*,LABEL=*                                                   
         SAM31                                                                  
         L     R0,MAXMSGLN                                                      
         AHI   R0,16               FOR START & END EYE CATCHER                  
         GETMAIN RU,LV=(0),LOC=(ANY,ANY),BNDRY=PAGE                             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   0(8,R1),=CL8'*MSGSTR*'                                           
         AHI   R1,8                                                             
         ST    R1,AMSGBUFF                                                      
         A     R1,MAXMSGLN                                                      
         AHI   R1,-16                                                           
         MVC   0(8,R1),=CL8'*MSGEND*'                                           
         SAM24                                                                  
*                                                                               
         PRNT  GOT_MSG_BUFFER,PRINT=ALWAYS                                      
*                                                                               
         MVC   GETBUFF_COA,AMSGBUFF                                             
         MVC   GETBUFF_DELNOT,AMSGBUFF                                          
         MVC   PUT1BUFF_WORKQ,AMSGBUFF                                          
         MVC   PUTBUFF_LOG,AMSGBUFF                                             
         MVC   PUTBUFF_LOG2,AMSGBUFF                                            
         MVC   PUTBUFF_LOGB,AMSGBUFF                                            
*                                                                               
         BLDL  0,ENTRYPTS            BUILD LIST OF APPC/MVS ENTRY PTS           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                  BAD RETURN FROM BLDL MACRO                 
*                                                                               
         LOAD  DE=CSQBCONN                                                      
         ST    R0,CSQBCONN_STRUCTURE                                            
         LOAD  DE=CSQBOPEN                                                      
         ST    R0,CSQBOPEN_STRUCTURE_COA                                        
         ST    R0,CSQBOPEN_STRUCTURE_LOG                                        
         ST    R0,CSQBOPEN_STRUCTURE_LOG2                                       
         ST    R0,CSQBOPEN_STRUCTURE_LOGB                                       
         ST    R0,CSQBOPEN_STRUCTURE_DELNOT                                     
         LOAD  DE=CSQBGET                                                       
         ST    R0,CSQBGET_STRUCTURE_COA                                         
         ST    R0,CSQBGET_STRUCTURE_DELNOT                                      
         LOAD  DE=CSQBPUT                                                       
         ST    R0,CSQBPUT_STRUCTURE_LOG                                         
         ST    R0,CSQBPUT_STRUCTURE_LOG2                                        
         ST    R0,CSQBPUT_STRUCTURE_LOGB                                        
         LOAD  DE=CSQBPUT1                                                      
         ST    R0,CSQBPUT1_STRUCTURE                                            
         LOAD  DE=CSQBCOMM                                                      
         ST    R0,CSQBCOMM_STRUCTURE                                            
         LOAD  DE=CSQBCLOS                                                      
         ST    R0,CSQBCLOS_STRUCTURE_COA                                        
         ST    R0,CSQBCLOS_STRUCTURE_LOG                                        
         ST    R0,CSQBCLOS_STRUCTURE_LOG2                                       
         ST    R0,CSQBCLOS_STRUCTURE_LOGB                                       
         ST    R0,CSQBCLOS_STRUCTURE_DELNOT                                     
         LOAD  DE=CSQBDISC                                                      
         ST    R0,CSQBDISC_STRUCTURE                                            
*                                                                               
         LAY   R2,CSQBCONN_STRUCTURE                                            
         BRAS  RE,CALLMQ           CONNECT TO MQ QUEUE MANAGER                  
         CLC   COMPCODE,=A(MQCC_OK)                                             
         JNE   *+2                 NO ERROR IS ACCEPTABLE                       
*                                                                               
         MVC   OBJDESC_COA_OBJECTTYPE,=A(MQOT_Q)                                
         MVC   OBJDESC_COA_OBJECTNAME,QNAME_COA                                 
         MVC   OPEN_OPTIONS,=A(MQOO_INPUT_AS_Q_DEF)                             
         LAY   R2,CSQBOPEN_STRUCTURE_COA                                        
         BRAS  RE,CALLMQ           OPEN COA QUEUE                               
         CLC   COMPCODE,=A(MQCC_OK)                                             
         JNE   *+2                 NO ERROR IS ACCEPTABLE                       
*                                                                               
         MVC   OBJDESC_DELNOT_OBJECTTYPE,=A(MQOT_Q)                             
         MVC   OBJDESC_DELNOT_OBJECTNAME,QNAME_DELNOT                           
         MVC   OPEN_OPTIONS,=A(MQOO_INPUT_AS_Q_DEF)                             
         LAY   R2,CSQBOPEN_STRUCTURE_DELNOT                                     
         BRAS  RE,CALLMQ           OPEN DELNOT QUEUE                            
         CLC   COMPCODE,=A(MQCC_OK)                                             
         JNE   *+2                 NO ERROR IS ACCEPTABLE                       
*                                                                               
         CLC   QNAME_LOG,SPACES                                                 
         BE    SETMQB                                                           
         MVC   OBJDESC_LOG_OBJECTTYPE,=A(MQOT_Q)                                
         MVC   OBJDESC_LOG_OBJECTNAME,QNAME_LOG                                 
         MVC   OPEN_OPTIONS,=A(MQOO_OUTPUT+MQOO_SET_IDENTITY_CONTEXT)           
         LAY   R2,CSQBOPEN_STRUCTURE_LOG                                        
         BRAS  RE,CALLMQ           OPEN LOG QUEUE                               
         CLC   COMPCODE,=A(MQCC_OK)                                             
         JNE   *+2                 NO ERROR IS ACCEPTABLE                       
*                                                                               
         CLC   QNAME_LOG2,SPACES                                                
         BE    SETMQB                                                           
         MVC   OBJDESC_LOG2_OBJECTTYPE,=A(MQOT_Q)                               
         MVC   OBJDESC_LOG2_OBJECTNAME,QNAME_LOG2                               
         MVC   OPEN_OPTIONS,=A(MQOO_OUTPUT+MQOO_SET_IDENTITY_CONTEXT)           
         LAY   R2,CSQBOPEN_STRUCTURE_LOG2                                       
         BRAS  RE,CALLMQ           OPEN LOG QUEUE 2                             
         CLC   COMPCODE,=A(MQCC_OK)                                             
         JNE   *+2                 NO ERROR IS ACCEPTABLE                       
*                                                                               
SETMQB   CLC   QNAME_LOGB,SPACES   BLOCKCHAIN QUEUE                             
         BE    SETMQX                                                           
         MVC   OBJDESC_LOGB_OBJECTTYPE,=A(MQOT_Q)                               
         MVC   OBJDESC_LOGB_OBJECTNAME,QNAME_LOGB                               
         MVC   OPEN_OPTIONS,=A(MQOO_OUTPUT+MQOO_SET_IDENTITY_CONTEXT)           
         LAY   R2,CSQBOPEN_STRUCTURE_LOGB                                       
         BRAS  RE,CALLMQ           OPEN BLOCKCHAIN QUEUE                        
         CLC   COMPCODE,=A(MQCC_OK)                                             
         JNE   *+2                 NO ERROR IS ACCEPTABLE                       
*                                                                               
SETMQX   XIT1  ,                                                                
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
***********************************************************************         
* SET UP OPERATOR COMMUNICATIONS                                      *         
***********************************************************************         
SETOPS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         EXTRACT ACOMM,FIELDS=COMM                                              
         L     RF,ACOMM            SET UP OPERATOR COMMUNICATIONS               
         USING COMLIST,RF                                                       
         MVC   AOPERECB+1(3),COMECBPT+1                                         
         MVC   ECBLST,AOPERECB     A(ECB)                                       
         L     R2,COMCIBPT         GET A(CIB)                                   
         LA    R3,COMCIBPT         GET A(A(CIB))                                
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R2                                                       
         CLI   CIBVERB,CIBSTART    WAS THIS BROUGHT UP WITH 'START'?            
         BNE   NOSTART                                                          
         DROP  R2                                                               
         QEDIT ORIGIN=(R3),BLOCK=(R2)  YES -- FREE THE CIB                      
NOSTART  QEDIT ORIGIN=(R3),CIBCTR=1    NOW ALLOW MODIFIES                       
         XIT1  ,                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO WAIT UNTIL POSTED OR A MESSAGE ARRIVES TO COA QUEUE      *         
***********************************************************************         
WAIT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
WAIT020  XC    TIMERECB,TIMERECB   CLEAR TIMER ECB                              
         XC    GETECB,GETECB       CLEAR SIGNAL ECB                             
         MVC   MSGDESC_COA_CORRELID,MQCI_NONE                                   
         MVC   MSGDESC_COA_MSGID,MQMI_NONE                                      
         L     RF,=A(MQMO_NONE)                                                 
         ST    RF,GETMSGOPTS_MATCHOPTIONS                                       
*                                                                               
         L     RF,=A(MQGMO_SET_SIGNAL)                                          
         ST    RF,GETMSGOPTS_OPTIONS                                            
         LA    RF,GETECB           TELL IT TO SIGNAL AND SET ECB                
         ST    RF,GETMSGOPTS_SIGNAL1                                            
         L     RF,=A(MQWI_UNLIMITED)                                            
         ST    RF,GETMSGOPTS_WAITINTERVAL                                       
*                                                                               
WAIT040  LAY   R2,CSQBGET_STRUCTURE_COA                                         
         BRAS  RE,CALLMQ           CALL MQ TO FLAG WHEN MESSAGE ARRIVES         
         CLC   COMPCODE,=A(MQCC_OK)                                             
         BE    EXITOK              GOT A MESSAGE                                
         CLC   COMPCODE,=A(MQCC_WARNING)                                        
         BNE   *+16                                                             
         CLC   REASON,=A(MQRC_SIGNAL_REQUEST_ACCEPTED)                          
         BE    *+6                                                              
         DC    H'0'                NO OTHER WARNING IS ACCEPTABLE               
*                                                                               
*WE SHOULDN'T NEED TIMER IF MQ GET SIGNAL WORKS PROPERLY. YYUN,11/02/09         
*        BRAS  RE,SETTIMER                                                      
*                                                                               
WAIT060  WAIT  1,ECBLIST=ECBLST                                                 
*                                                                               
         TM    GETECB,X'40'        MQ SIGNALS MESSAGE ARRIVED                   
         BO    WAIT020             YES - GO GET IT                              
*                                                                               
         TM    TIMERECB,X'40'                                                   
         BO    WAIT020             TIMER POPPED, TRY GET AGAIN                  
*                                                                               
         OC    STIMER1,STIMER1                                                  
         BZ    WAIT080                                                          
         STIMERM CANCEL,ID=STIMER1                                              
         LTR   RF,RF               CANCEL TIMER                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WAIT080  L     RF,AOPERECB                                                      
         TM    0(RF),X'40'         OPERATOR INTERRUPT                           
         BZ    *+8                                                              
         BRAS  RE,CHKOPER                                                       
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR STOP REQUESTED?                     
         BE    EXITL               YES                                          
         B     WAIT060             NO - RESUME ON WAIT                          
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* THE OPERATOR HAS INTERRUPTED WITH EITHER A 'STOP' OR 'MODIFY'       *         
* COMMAND.  EXAMINE THE COMMAND AND TAKE THE APPROPRIATE ACTION.      *         
***********************************************************************         
CHKOPER  NTR1  BASE=*,LABEL=*                                                   
         PRNT  CheckOperator,PRINT=ALWAYS                                       
*                                                                               
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         L     R2,COMCIBPT         A(CIB)                                       
         LA    R3,COMCIBPT         A(A(CIB))                                    
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R2                                                       
         CLI   CIBVERB,CIBSTOP     DID OPERATOR ENTER 'STOP'?                   
         BNE   CH02                                                             
         MVI   OPERSTOP,C'Y'       YES -- SET STOP FLAG                         
         PRNT  OperatorStop,PRINT=ALWAYS                                        
         B     CHX                                                              
*                                                                               
CH02     CLI   CIBVERB,CIBMODFY    DID OPERATOR ENTER 'MODIFY'?                 
         BE    *+6                 YES                                          
         DC    H'0'                WHAT DID THE OPERATOR DO?                    
*                                                                               
         B     CHX                 NO MODIFY COMMAND YET, JUST EXIT             
*                                                                               
CHX      L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         LA    R3,COMCIBPT         A(A(CIB))                                    
         QEDIT ORIGIN=(R3),BLOCK=(R2)  FREE THE CIB                             
         B     EXIT                                                             
         DROP  RF                                                               
*                                                                               
***********************************************************************         
*SET TIMER                                                            *         
***********************************************************************         
SETTIMER NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         PRNT  SetTimer,PRINT=ALWAYS                                            
*                                                                               
         OC    STIMER1,STIMER1                                                  
         BZ    ST010                                                            
         STIMERM CANCEL,ID=STIMER1                                              
         LTR   RF,RF               CANCEL TIMER                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ST010    STIMERM SET,ID=STIMER1,BINTVL=WAITSECS,EXIT=TIMERXIT                   
         LTR   RF,RF               WAIT 2 HOURS                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
STIMERX  XIT1                                                                   
*                                                                               
WAITSECS DC    A(3600*100*2)       WAIT 7200 SEC (2 HOURS)                      
STIMER1  DS    XL4                 FOR TIMER POPS                               
*                                                                               
TIMERXIT SAVE  (14,12),,*                                                       
         LR    RB,RF                                                            
         USING TIMERXIT,RB                                                      
         POST  TIMERECB                                                         
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
         SPACE 2                                                                
TIMERECB DS    F                                                                
         SPACE 3                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* UPON ENTRY,  R2 POINTS TO PARAMETER STRUCTURE                                 
* UPON RETURN, COMPCODE CONTAINS THE MQ COMPLETION CODE                         
*              REASON CONTAINS THE MQ REASON CODE                               
***********************************************************************         
CALLMQ   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SAM31                                                                  
         L     RF,0(R2)            RF = A(MQ ROUTINE)                           
         LA    R3,24(R2)           R3 = A(PARAMETER LIST)                       
         CALL  (15),MF=(E,(R3))                                                 
         SAM24                                                                  
*                                                                               
         MVI   P,C'+'              '+' MEANS IT'S AN MQ CALL                    
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODES          
         MVC   P+30(8),=C'COMP. OK'                                             
*                                                                               
         C     R2,=A(CSQBPUT1_STRUCTURE)                                        
         BNE   *+10                                                             
         MVC   P+40(40),OBJDESC_WORKQ_OBJECTNAME                                
*                                                                               
         C     R2,=A(CSQBPUT_STRUCTURE_LOG)                                     
         BNE   *+10                                                             
         MVC   P+40(40),OBJDESC_LOG_OBJECTNAME                                  
*                                                                               
         C     R2,=A(CSQBPUT_STRUCTURE_LOG2)                                    
         BNE   *+10                                                             
         MVC   P+40(40),OBJDESC_LOG2_OBJECTNAME                                 
*                                                                               
         C     R2,=A(CSQBPUT_STRUCTURE_LOGB)                                    
         BNE   *+10                                                             
         MVC   P+40(40),OBJDESC_LOGB_OBJECTNAME                                 
*                                                                               
         CLC   COMPCODE,=A(MQCC_OK)                                             
         BE    CALLMQ30                                                         
         MVC   P+30(8),=C'WARNING!'                                             
         CLC   COMPCODE,=A(MQCC_WARNING)                                        
         BE    CALLMQ10                                                         
         MVC   P+30(8),=C'*FAILED!'                                             
         CLC   COMPCODE,=A(MQCC_FAILED)                                         
         JNE   *+2                 UNKNOWN COMPLETION CODE                      
*                                                                               
CALLMQ10 MVC   P+120(9),=C'*** ERROR'                                           
         MVC   P+79(7),=C'REASON='                                              
         EDIT  REASON,(5,P+87),ZERO=NOBLANK                                     
         MVI   P+92,C':'                                                        
*                                                                               
         L     RF,=A(MQ_REASON_CODE_TABLE)                                      
CALLMQ20 CLI   0(RF),X'FF'         END OF TABLE?                                
         BNE   *+14                NO                                           
         MVC   P+94(22),=C'*** UNKNOWN REASON ***'                              
         B     CALLMQ25            REASON CODE NOT IN TABLE                     
         CLC   REASON,0(RF)        FIND MATCH ON REASON CODE                    
         BE    *+12                GOT IT                                       
         LA    RF,28(RF)           BUMP TO NEXT TABLE ENTRY                     
         B     CALLMQ20                                                         
*                                                                               
         MVC   P+94(24),4(RF)                                                   
*                                                                               
CALLMQ25 CLC   COMPCODE,=A(MQCC_OK)                                             
         BE    CALLMQ30                                                         
         CLC   COMPCODE,=A(MQCC_WARNING) CLARIFY IF MESSAGE NOT FOUND           
         BNE   CALLMQ30                                                         
         CLC   REASON,=A(MQRC_SIGNAL_REQUEST_ACCEPTED)                          
         JNE   *+2                                                              
         MVC   WORK(40),P+79                                                    
         MVC   P+30(100),SPACES                                                 
         MVC   P+30(20),=CL20'NO MESSAGE AVAILABLE'                             
         MVC   P+55(40),WORK                                                    
*                                                                               
CALLMQ30 EQU   *                                                                
         PRNT                                                                   
*                                                                               
CALLMQX  XIT1  ,                                                                
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
***********************************************************************         
* VALIDATE THE COA MESSAGE                                            *         
***********************************************************************         
VALCOA   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   P+30(L'MSGDESC_COA_CORRELID),MSGDESC_COA_CORRELID                
         PRNT  InCOA                                                            
*                                                                               
         CLC   MSGDESC_DELNOT_CORRELID,MSGDESC_COA_CORRELID                     
         BE    VCOA60              SAME AS LAST CORRELID, SKIP IT!              
*                                                                               
*        CLC   MSGDESC_COA_MSGTYPE,=A(MQMT_DATAGRAM)                            
*        BNE   VCOA50                                                           
         CLC   MSGDESC_COA_FEEDBACK,=A(MQFB_COA)                                
         BNE   VCOA50                                                           
         CLI   MSGDESC_COA_CORRELID,C'1'      CHECK VERSION NUMBER              
         BNE   VCOA50                                                           
         CLC   MSGDESC_COA_CORRELID+1(09),=CL09'NO DLNNOT'                      
         BE    VCOA30                         NO DLNNOT FOR THIS COA            
         B     EXITOK              GOT A VALID COA MESSAGE                      
*                                                                               
VCOA30   EQU   *                                                                
         B     EXITL               SKIP THIS COA                                
*                                                                               
VCOA50   EQU   *                                                                
         MVC   P+30(L'MSGDESC_COA_MSGTYPE),MSGDESC_COA_MSGTYPE                  
         PRNT  Msgtype                                                          
         MVC   P+30(L'MSGDESC_COA_FEEDBACK),MSGDESC_COA_FEEDBACK                
         PRNT  Feedback                                                         
         MVC   P+30(L'MSGDESC_COA_CORRELID),MSGDESC_COA_CORRELID                
         PRNT  BadCOA                                                           
         B     EXITL               SKIP THIS COA                                
*                                                                               
VCOA60   MVC   P+30(L'MSGDESC_COA_CORRELID),MSGDESC_COA_CORRELID                
         PRNT  DuplicateCOA                                                     
         B     EXITL               SKIP THIS COA                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET A SPECIFIC DELNOT FROM DELNOT QUEUE                  *         
***********************************************************************         
GET1DNT  NTR1  BASE=*,LABEL=*                                                   
*                                  MATCH ON CORRELID FROM COA                   
G1DNT10  MVC   MSGDESC_DELNOT_CORRELID,MSGDESC_COA_CORRELID                     
         MVC   MSGDESC_DELNOT_MSGID,MQMI_NONE                                   
         MVC   GETMSGOPTS_MATCHOPTIONS,=A(MQMO_MATCH_CORREL_ID)                 
*                                                                               
         MVC   GETMSGOPTS_OPTIONS,=A(MQGMO_WAIT)                                
         L     RF,GDNTWAIT         WAIT SOME SECONDS FOR DELNOT                 
         ST    RF,GETMSGOPTS_WAITINTERVAL                                       
*                                                                               
         LAY   R2,CSQBGET_STRUCTURE_DELNOT                                      
         BRAS  RE,CALLMQ           CALL MQ TO FLAG WHEN MESSAGE ARRIVES         
         CLC   COMPCODE,=A(MQCC_OK)                                             
         BNE   G1DNT40             FAILED                                       
*                                  GOT A MESSAGE                                
         CLC   MSGDESC_DELNOT_MSGID(6),=CL6'NOTNOT'                             
         BNE   EXITOK              NO - PROCESS IT                              
*                                  YES- DUMMY DLNNOT - IGNORE IT                
         MVC   P+30(L'MSGDESC_DELNOT_CORRELID),MSGDESC_DELNOT_CORRELID          
         MVC   P+60(L'MSGDESC_DELNOT_MSGID),MSGDESC_DELNOT_MSGID                
         PRNT  NotNotFound                                                      
         B     EXITL                                                            
*                                  TIMED OUT                                    
G1DNT40  BRAS  RE,MISDNT           MISSING DELNOT                               
         B     EXITL               RETURN NEQ                                   
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* MISSING THE DELNOT  - WHY?                                          *         
* 1)RECEIVED MULTIPLE COA W/ THE SAME CORRELID, THAT HAPPENED BEFORE. *         
* 2)RECEIVED COA TOO EARLY, DELAY WHEN DARE SENDER PUTS OUT DELNOT.   *         
* 3)RUBBISH COA.                                                      *         
***********************************************************************         
MISDNT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   P+30(L'MSGDESC_COA_CORRELID),MSGDESC_COA_CORRELID                
         PRNT  DNTNotFound                                                      
*                                                                               
         CLC   MSGDESC_COA_PUTAPPLNAME,QMGRNAME                                 
         BNE   MDST20                                                           
         PRNT  IgnoreMyQMgrCOA                                                  
         B     MDSTX               IGNORE COA FROM MY OWN QMGR                  
*                                                                               
MDST20   DS    0H                                                               
**********************************************************************          
* ON 10/26, WE GOT 116 SAME COA FROM A PARTNER.                                 
* COMPARE TO THE LAST CORRELID                                                  
* IF THE SAME, WTOR TO CALL YYUN/SKUI,                                          
* THEN REPLY TO RESUME W/O CHECKING AGAIN....   -10/30/09, YYUN                 
*                                                                               
         CLI   SVCOA,X'FF'                                                      
         BE    COAEXIT             DISABLE CHECKING                             
         CLC   MSGDESC_COA_CORRELID,SVCOA                                       
         BNE   COASAVE             IGNORE THIS ONE                              
         GOTO1 =V(LOGIO),DMCB,1,=C'SAME COA! CONTACT MF FACILITIES    '         
*                                          REPLY ANYTHING                       
         GOTO1 =V(LOGIO),DMCB,0,(6,WORK)   NO NEED TO CHECK THE REPLY           
         MVI   SVCOA,X'FF'                                                      
         B     COAEXIT             DISABLE CHECKING FROM NOW                    
COASAVE  DS    0H                                                               
         MVC   SVCOA,MSGDESC_COA_CORRELID                                       
COAEXIT  DS    0H                                                               
**********************************************************************          
MDSTX    XIT1  ,                                                                
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* PUT THE DELNOT TO THE CORRESPONDING WORKQ (& LOGQ IF NEEDED)        *         
***********************************************************************         
PUT1WRKQ NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RF,MQPMO_SYNCPOINT                                               
         ICM   RE,15,=AL4(MQPMO_FAIL_IF_QUIESCING)                              
         AR    RF,RE                                                            
         ST    RF,PUTMSGOPTS_OPTIONS                                            
*                                                                               
         MVC   OBJDESC_WORKQ_OBJECTTYPE,=A(MQOT_Q)                              
         BRAS  RE,GETWRKQ          GET QNAME FOR WORKQ                          
*                                                                               
*                                  COPY CORRELID + MSGID                        
         MVC   MSGDESC_WORKQ_CORRELID,MSGDESC_DELNOT_CORRELID                   
         MVC   MSGDESC_WORKQ_MSGID,MSGDESC_DELNOT_MSGID                         
*                                  DELNOT ALREADY IN BUFFER PREV GET            
         LAY   R2,CSQBPUT1_STRUCTURE                                            
         BRAS  RE,CALLMQ           PUT DELNOT TO WORKQ                          
         CLC   COMPCODE,=A(MQCC_OK)                                             
         BE    *+6                                                              
         DC    H'0'                NO ERROR IS ACCEPTABLE                       
*                                                                               
         CLC   QNAME_LOG,SPACES                                                 
         BH    P1WKQ10                                                          
         CLC   QNAME_LOGB,SPACES                                                
         BNH   P1WKQX                                                           
P1WKQ10  L     R1,PUTBUFF_LOG     OVERRIDE '+++DARE SEND TIME=HHMMSS'           
         SAM31 ,                    WITH   '+++DARE SEND LOG =HHMMSS'           
         MVC   13(4,R1),=C'LOG '                                                
         SAM24                                                                  
*                                  COPY CORRELID + MSGID                        
         MVC   MSGDESC_LOG_CORRELID,MSGDESC_DELNOT_CORRELID                     
         MVC   MSGDESC_LOG_MSGID,MSGDESC_DELNOT_MSGID                           
*                                  USE USERID FIELD FOR PARNTER CODE            
         MVC   MSGDESC_LOG_USERIDENTIFIER(4),=CL4'COA '                         
*                                                                               
         CLC   QNAME_LOG,SPACES                                                 
         BNH   P1WKQ20                                                          
*                                                                               
         LAY   R2,CSQBPUT_STRUCTURE_LOG                                         
         BRAS  RE,CALLMQ           PUT DELNOT TO LOGQ1                          
         CLC   COMPCODE,=A(MQCC_OK)                                             
         JNE   *+2                 NO ERROR IS ACCEPTABLE                       
*                                                                               
         CLC   QNAME_LOG2,SPACES                                                
         BNH   P1WKQ20                                                          
         LAY   R2,CSQBPUT_STRUCTURE_LOG2                                        
         BRAS  RE,CALLMQ           PUT DELNOT TO LOGQ 2                         
         CLC   COMPCODE,=A(MQCC_OK)                                             
         JNE   *+2                 NO ERROR IS ACCEPTABLE                       
*                                                                               
P1WKQ20  CLC   QNAME_LOGB,SPACES                                                
         BNH   P1WKQX                                                           
*                                                                               
         L     R2,=A(SSB)                                                       
         LAM   AR2,AR2,SSOTBLET-SSOOFF(R2)                                      
         XR    R2,R2                                                            
         SAM31                                                                  
         SAC   512                                                              
         ICM   R2,15,TABSCOMM-TABSADDR(R2)                                      
         USING TCOMMOND,R2                                                      
         CLI   TCOBCMQ,C'N'        BLOCKCHAIN OUTPUT STOPPED?                   
         DROP  R2                                                               
         SAM24                                                                  
         SAC   0                                                                
         LAM   AR0,ARF,=16F'0'                                                  
         BE    P1WKQX              DO NOT OUTPUT TO BLOCKCHAIN QUEUE            
*                                                                               
         LAY   R2,CSQBPUT_STRUCTURE_LOGB                                        
         BRAS  RE,CALLMQ           PUT DELNOT TO BLOCKCHAIN QUEUE               
         CLC   COMPCODE,=A(MQCC_OK)                                             
         JNE   *+2                 NO ERROR IS ACCEPTABLE                       
*                                                                               
P1WKQX   XIT1  ,                                                                
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
***********************************************************************         
* FIND WORKQ NAME FOR THIS DELNOT                                     *         
***********************************************************************         
GETWRKQ  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   OBJDESC_WORKQ_OBJECTNAME,SPACES                                  
         LA    R1,OBJDESC_WORKQ_OBJECTNAME   'DARE.SSSS.TTT.WORK.QUEUE'         
         MVC   0(5,R1),=CL5'DARE.'                                              
         AHI   R1,5                                                             
*                                                                               
         L     RF,=A(SSB)                                                       
         CLI   SSODSPAC-SSOOFF(RF),C'T'                                         
         BNE   GWRKQ01                                                          
         MVC   0(5,R1),=CL5'TEST.'                                              
         AHI   R1,5                                                             
         B     GWRKQ20                                                          
GWRKQ01  CLI   SSODSPAC-SSOOFF(RF),C'Q'                                         
         BNE   GWRKQ02                                                          
         MVC   0(4,R1),=CL4'FQA.'                                               
         AHI   R1,4                                                             
         B     GWRKQ20                                                          
GWRKQ02  EQU   *                                                                
         MVC   0(5,R1),=CL5'PROD.'                                              
         AHI   R1,5                                                             
GWRKQ20  MVC   0(3,R1),MSGDESC_DELNOT_MSGID+19    TASK ID TO SEND               
         MVI   3(R1),C'.'                                                       
         AHI   R1,3+1                                                           
         MVC   0(10,R1),=C'WORK.QUEUE'                                          
*                                                                               
         XIT1  ,                                                                
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                               
COMMWORK DS    0D                  COMMON STORAGE AREA                          
*                                                                               
EXITL    CLI   *,255               SET CC LOW                                   
         B     EXIT                                                             
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
EXIT     XIT1  ,                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'***SSB**'                                                    
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
         SPACE 3                                                                
         DC    CL8'***UTL**'                                                    
UTL      DC    F'0',X'0A'          FOR DATAMGR (CONTROL SYSTEM)                 
*                                                                               
         DS    0D                                                               
         DC    CL08'ECBLIST*'                                                   
ECBLST   DC    A(0)                A(OPERATOR ECB IS STORED HERE)               
         DC    A(GETECB)                                                        
         DC    X'80',AL3(TIMERECB) TIMER POP                                    
         DC    CL08'ECBLISTX'                                                   
*                                                                               
GETECB   DC    A(0)                COA QUEUE ECB                                
AOPERECB DC    A(0)                A(ECB OF OPERATOR INTERRUPT)                 
ACOMM    DC    A(0)                A(COMMUNICATIONS PARAMETER LIST)             
OPERSTOP DC    C'N'                'Y' = OPERATOR ENTERED 'STOP'                
*                                                                               
         CMQA LIST=YES,EQUONLY=NO                                               
         EJECT                                                                  
* MQSERIES CALL PARAMETERS                                                      
*                                                                               
QMGRNAME       DC      CL48' '                                                  
QNAME_COA      DC      CL48' '                                                  
QNAME_DELNOT   DC      CL48' '                                                  
QNAME_LOG      DC      CL48' '                                                  
QNAME_LOG2     DC      CL48' '                                                  
QNAME_LOGB     DC      CL48' '                                                  
*                                                                               
HCONN          DS      F         MQ QMGR CONNECTION HANDLE                      
HOBJ_COA       DS      F         OBJECT HANDLE                                  
HOBJ_LOG       DS      F         OBJECT HANDLE                                  
HOBJ_LOG2      DS      F         OBJECT HANDLE                                  
HOBJ_LOGB      DS      F         OBJECT HANDLE                                  
HOBJ_DELNOT    DS      F         OBJECT HANDLE                                  
OPEN_OPTIONS   DS      F         MQOPEN OPTIONS                                 
CLOSE_OPTIONS  DS      F         MQCLOSE OPTIONS                                
COMPCODE       DS      F         COMPLETION CODE                                
REASON         DS      F         QUALIFIES COMPLETION CODE                      
DATALENGTH     DS      F         LENGTH OF THE MESSAGE                          
*                                                                               
               DS    0F                                                         
               DC    CL16'MSGBUFF ADDR='                                        
AMSGBUFF       DC    A(0)               A(MESSAGE BUFFER)                       
MAXMSGLN       DC    A(2*1024*1024)     MAX MESSAGE LENGTH(2M)                  
*                                                                               
OBJDESC_COA    CMQODA  LIST=YES  OBJECT DESCRIPTOR                              
OBJDESC_LOG    CMQODA  LIST=YES  OBJECT DESCRIPTOR                              
OBJDESC_LOG2   CMQODA  LIST=YES  OBJECT DESCRIPTOR                              
OBJDESC_LOGB   CMQODA  LIST=YES  OBJECT DESCRIPTOR                              
OBJDESC_DELNOT CMQODA  LIST=YES  OBJECT DESCRIPTOR                              
OBJDESC_WORKQ  CMQODA  LIST=YES  OBJECT DESCRIPTOR                              
MSGDESC_COA    CMQMDA  LIST=YES  MESSAGE DESCRIPTOR                             
MSGDESC_LOG    CMQMDA  LIST=YES  MESSAGE DESCRIPTOR                             
         ORG   MSGDESC_LOG_FORMAT                                               
         DC    CL8'MQSTR   '     MQFMT_STRING  FOR DATA FORMAT                  
         ORG                                                                    
MSGDESC_DELNOT CMQMDA  LIST=YES  MESSAGE DESCRIPTOR                             
MSGDESC_WORKQ  CMQMDA  LIST=YES  MESSAGE DESCRIPTOR                             
         ORG   MSGDESC_WORKQ_FORMAT                                             
         DC    CL8'MQSTR   '     MQFMT_STRING  FOR DATA FORMAT                  
         ORG                                                                    
GETMSGOPTS     CMQGMOA LIST=YES  GET MESSAGE OPTIONS                            
PUTMSGOPTS     CMQPMOA LIST=YES  PUT MESSAGE OPTIONS                            
PUTMSGOPTS_LOG CMQPMOA LIST=YES  PUT MESSAGE OPTIONS                            
         ORG   PUTMSGOPTS_LOG_OPTIONS                                           
         DC    A(MQPMO_SET_IDENTITY_CONTEXT)                                    
         ORG                                                                    
         EJECT                                                                  
DMCB     DS    10F                                                              
DUB      DS    D                                                                
PRNTDUB  DS    D                                                                
CARD     DS    CL80                                                             
WORK     DS    CL80                                                             
GDNTWAIT DC    A(2*1000)           2 SEC WAIT TIME FOR MQGET                    
SVCOA    DS    CL(L'MSGDESC_COA_CORRELID)    SAVED COA CORRELID                 
PRNTTIME DS    CL9                                                              
TRACEFLG DC    C'Y'                'Y' TO PRINT DETAILED TRACE                  
         SPACE 3                                                                
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
CSQBPUT1 DC    CL8'CSQBPUT1'                                                    
         DC    XL52'00'                                                         
*                                                                               
ENTRYLSQ EQU   *                                                                
         EJECT                                                                  
* PARAMETER LISTS FOR MQSERIES CALLS                                            
*  F    A(ROUTINE)                                                              
*  CL16 EBCDIC ROUTINE NAME                                                     
*  XL1  FLAGS                                                                   
*  XL3  SPARE                                                                   
*  PARAMETERS (STANDARD IBM FORMAT)                                             
*                                                                               
CSQBCONN_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_Connect'                                
                          DC    A(0)                                            
                          DC    A(QMGRNAME)                                     
                          DC    A(HCONN)                                        
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBOPEN_STRUCTURE_COA    DS    A                                               
                          DC    CL16'MQ_OpenCOA'                                
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(OBJDESC_COA)                                  
                          DC    A(OPEN_OPTIONS)                                 
                          DC    A(HOBJ_COA)                                     
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBOPEN_STRUCTURE_LOG    DS    A                                               
                          DC    CL16'MQ_OpenLog'                                
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(OBJDESC_LOG)                                  
                          DC    A(OPEN_OPTIONS)                                 
                          DC    A(HOBJ_LOG)                                     
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBOPEN_STRUCTURE_LOG2   DS    A                                               
                          DC    CL16'MQ_OpenLog2'                               
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(OBJDESC_LOG2)                                 
                          DC    A(OPEN_OPTIONS)                                 
                          DC    A(HOBJ_LOG2)                                    
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBOPEN_STRUCTURE_LOGB   DS    A                                               
                          DC    CL16'MQ_OpenLogB'                               
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(OBJDESC_LOGB)                                 
                          DC    A(OPEN_OPTIONS)                                 
                          DC    A(HOBJ_LOGB)                                    
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBOPEN_STRUCTURE_DELNOT DS    A                                               
                          DC    CL16'MQ_OpenDelNot'                             
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(OBJDESC_DELNOT)                               
                          DC    A(OPEN_OPTIONS)                                 
                          DC    A(HOBJ_DELNOT)                                  
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBGET_STRUCTURE_COA     DS    A                                               
                          DC    CL16'MQ_GetCOA'                                 
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(HOBJ_COA)                                     
                          DC    A(MSGDESC_COA)                                  
                          DC    A(GETMSGOPTS)                                   
                          DC    A(MAXMSGLN)                                     
GETBUFF_COA               DC    A(0)                                            
                          DC    A(DATALENGTH)                                   
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBGET_STRUCTURE_DELNOT  DS    A                                               
                          DC    CL16'MQ_GetDelNot'                              
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(HOBJ_DELNOT)                                  
                          DC    A(MSGDESC_DELNOT)                               
                          DC    A(GETMSGOPTS)                                   
                          DC    A(MAXMSGLN)                                     
GETBUFF_DELNOT            DC    A(0)                                            
                          DC    A(DATALENGTH)                                   
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBPUT_STRUCTURE_LOG     DS    A                                               
                          DC    CL16'MQ_PutLog'                                 
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(HOBJ_LOG)                                     
                          DC    A(MSGDESC_LOG)                                  
                          DC    A(PUTMSGOPTS_LOG)                               
                          DC    A(DATALENGTH)                                   
PUTBUFF_LOG               DC    AL4(0)                                          
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBPUT_STRUCTURE_LOG2    DS    A                                               
                          DC    CL16'MQ_PutLog2'                                
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(HOBJ_LOG2)                                    
                          DC    A(MSGDESC_LOG)                                  
                          DC    A(PUTMSGOPTS_LOG)                               
                          DC    A(DATALENGTH)                                   
PUTBUFF_LOG2              DC    AL4(0)                                          
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBPUT_STRUCTURE_LOGB    DS    A                                               
                          DC    CL16'MQ_PutLogB'                                
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(HOBJ_LOGB)                                    
                          DC    A(MSGDESC_LOG)                                  
                          DC    A(PUTMSGOPTS_LOG)                               
                          DC    A(DATALENGTH)                                   
PUTBUFF_LOGB              DC    AL4(0)                                          
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBPUT1_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_Put1WorkQ'                              
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(OBJDESC_WORKQ)                                
                          DC    A(MSGDESC_WORKQ)                                
                          DC    A(PUTMSGOPTS)                                   
                          DC    A(DATALENGTH)                                   
PUT1BUFF_WORKQ            DC    AL4(0)                                          
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCOMM_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_Commit'                                 
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCLOS_STRUCTURE_COA    DS    A                                               
                          DC    CL16'MQ_CloseCOA'                               
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(HOBJ_COA)                                     
                          DC    A(CLOSE_OPTIONS)                                
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCLOS_STRUCTURE_LOG    DS    A                                               
                          DC    CL16'MQ_CloseLog'                               
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(HOBJ_LOG)                                     
                          DC    A(CLOSE_OPTIONS)                                
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCLOS_STRUCTURE_LOG2   DS    A                                               
                          DC    CL16'MQ_CloseLog2'                              
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(HOBJ_LOG2)                                    
                          DC    A(CLOSE_OPTIONS)                                
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCLOS_STRUCTURE_LOGB   DS    A                                               
                          DC    CL16'MQ_CloseLogB'                              
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(HOBJ_LOGB)                                    
                          DC    A(CLOSE_OPTIONS)                                
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCLOS_STRUCTURE_DELNOT DS    A                                               
                          DC    CL16'MQ_CloseDelNot'                            
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(HOBJ_DELNOT)                                  
                          DC    A(CLOSE_OPTIONS)                                
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBDISC_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_Disconnect'                             
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
       ++INCLUDE DDMQREASON                                                     
* DDDPRINT                                                                      
* FATABSD                                                                       
* FATABSCOM                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE FATABSD                                                        
       ++INCLUDE FATABSCOM                                                      
         DCBD  DSORG=PS,DEVD=DA                                                 
         PRINT ON                                                               
* IEZCIB                                                                        
* IEZCOM                                                                        
         PRINT OFF                                                              
         DSECT                                                                  
         IEZCIB                                                                 
         IEZCOM                                                                 
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'191DDDARECOA 02/28/19'                                      
         END                                                                    
