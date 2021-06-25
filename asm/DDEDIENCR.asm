*          DATA SET DDEDIENCR  AT LEVEL 014 AS OF 01/12/06                      
*PHASE EDIENCRA                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE EDISUB                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE BINSR31                                                                
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        EDIENCR -- RECEIVE ENCODA TRANSMISSIONS VIA APPC/MVS *         
*                           (SAME PROCESS AS BIAS RECEIVER)           *         
*                                                                     *         
*  CALLED BY:    DDEDIENCS (EDICT SENDER TO ENCODA)                   *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- WORK                                           *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- PARAMETERS FROM EDIENCS (VIA R1)               *         
*                R7 -- S P A R E                                      *         
*                R8 -- COMMON STORAGE AREA (2ND BASE)                 *         
*                R9 -- S P A R E                                      *         
*                RA -- CPRINT                                         *         
*                RB -- PROGRAM BASE                                   *         
*                RC -- COMMON STORAGE AREA                            *         
*                RD -- REGISTER SAVE AREA                             *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'DDEDIENCR -- RECEIVE ENCODA TRANSMISSIONS VIA APPC/MVS'         
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
EDIENCR  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,EDIENCR*,=A(R13CHAIN)                                          
*                                                                               
         LR    R6,RC                                                            
         L     RC,=A(COMMWORK)     COMMON STORAGE AREA                          
         LR    R8,RC                                                            
         AH    R8,=H'4096'                                                      
         USING COMMWORK,RC,R8                                                   
*                                                                               
         SH    R6,=H'4'                                                         
         L     R6,0(R6)                                                         
         L     R6,0(R6)            A(R1 PARAMETERS FROM ATTACH)                 
         USING ENRPARMD,R6                                                      
*                                                                               
         MVC   TRACEFLG,ENRTRACE   'Y' = PRINT DETAILED TRACE                   
         ENTRY TRACEFLG                                                         
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         L     RF,=V(PRNTER)       A(SYSPRINT DCB)                              
         MVC   DCBDDNAM-IHADCB(8,RF),=C'ENRTRACE'  DDNAME=ENRTRACE              
*                                                                               
         MVC   TITLE(30),=C'EDICT: ENCODA RECEIVER SUBTASK'                     
         PRNT  START_SUBTASK,PRINT=ALWAYS                                       
*                                                                               
         GOTO1 =A(INITIAL),(RC)    INITIALIZE                                   
         EJECT                                                                  
         LA    R2,ATBRFA2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   REGISTER FOR ALLOCATES                       
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    UNREGSTR                                                         
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    RCVALLOC            WE ARE REGISTERED FOR ALLOCATES              
         CLC   RETURN_CODE,ATBCTS_WARNING                                       
         BNE   *+18                                                             
         CLC   REASON_CODE,ATBCTS_ALREADY_REGISTERED                            
         BE    RCVALLOC            WE WERE ALREADY REGISTERED, SO OK            
         B     CANTREG                                                          
         CLC   RETURN_CODE,ATBCTS_REQUEST_UNSUCCESSFUL                          
         BNE   CANTREG                                                          
         CLC   REASON_CODE,ATBCTS_INVAL_LOCAL_LU                                
         BNE   CANTREG             WE CAN'T REGISTER                            
*                                                                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'EDICT01 *JDS/ENCODA OPEN P+        
               ARAMETER ERROR. PROBABLE CAUSE: UNDEFINED LOCAL LUNAME'          
         ABEND 705,DUMP                                                         
*                                                                               
CANTREG  MVC   P+30(14),=C'REASON CODE = '                                      
         EDIT  REASON_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         PRNT  COULDNT_REGISTER,PRINT=ALWAYS                                    
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'EDICT01 *JDS/ENCODA APPC E+        
               RROR. COULD NOT REGISTER FOR ALLOCATES.'                         
         ABEND 705,DUMP                                                         
*                                                                               
RCVALLOC GOTO1 =A(QUERYALQ),(RC)   QUERY ALLOCATE QUEUE                         
*                                                                               
         LA    R2,ATBRAL2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   RECEIVE ALLOCATE                             
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    UNREGSTR            YES                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    INCONV              WE HAVE A CONVERSATION                       
         CLC   RETURN_CODE,ATBCTS_REQUEST_UNSUCCESSFUL                          
         BNE   NOCONV                                                           
         CLC   REASON_CODE,ATBCTS_NO_ALLOC_TO_RECEIVE                           
         BNE   NOCONV                                                           
*                                                                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'EDICT01 *JDS/ENCODA HAS NO+        
               T YET ALLOCATED ITS SENDER'                                      
         B     RCVALLOC            TRY TO RECEIVE ALLOCATE AGAIN                
*                                                                               
NOCONV   PRNT  NO_CONVERSATION,PRINT=ALWAYS                                     
         B     UNREGSTR                                                         
*                                                                               
INCONV   PRNT  OPENENCODARCVR,PRINT=ALWAYS                                      
*                                                                               
         GOTO1 =A(QUERYALQ),(RC)   QUERY ALLOCATE QUEUE                         
         GOTO1 =A(PRNTATTB),(RC)   PRINT CONVERSATION ATTRIBUTES                
*                                                                               
         MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
         LA    R2,ATBRCVW_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   RECEIVE AND WAIT                             
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    UNREGSTR                                                         
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         CLC   STATUS_RECEIVED,ATB_CONFIRM_RECEIVED                             
         BE    *+6                                                              
         DC    H'0'                I EXPECT A CONFIRMATION REQUEST              
*                                                                               
         LA    R2,ATBCFMD_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   ISSUE CONFIRMATION                           
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    UNREGSTR                                                         
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
         LA    R2,ATBRCVW_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   RECEIVE AND WAIT                             
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    UNREGSTR                                                         
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         CLC   STATUS_RECEIVED,ATB_SEND_RECEIVED                                
         BNE   BADSHAKE                                                         
         CLC   APPCM1(APPCM1Q),BUFFER                                           
         BNE   BADSHAKE            ENCODA DID NOT RESPOND READY                 
         CLC   APPCM1A(APPCM1AQ),BUFFER+19                                      
         BNE   BADSHAKE            WRONG MODE OR VERSION NUMBER                 
*                                                                               
         PRNT  ABOUTTORESPOND,PRINT=ALWAYS                                      
         XC    BUFFER,BUFFER                                                    
         LH    R3,=Y(APPCM2AQ)                                                  
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   BUFFER(0),APPCM2    INITIAL SIGNON                               
         GOTO1 =A(DATETIME),(RC)   PUT DATE/TIME INTO MESSAGE                   
         MVC   BUFFER+47(8),YYYYMMDD                                            
         MVC   BUFFER+61(6),HHMMSS                                              
         LA    R3,1(R3)            R3 = L'DATA                                  
         LR    RF,R3                                                            
         LA    RF,4(RF)            ADD 2 FOR CRLF AND LENGTH                    
         LA    RE,BUFFER_LENGTH                                                 
         ST    RE,DMCB+4           FOR PRNTBL                                   
         STH   RF,BUFFER_LENGTH                                                 
         ST    RF,SEND_LENGTH                                                   
         LR    R2,RF               SAVE LENGTH (FOR PRNTBL)                     
         LA    RF,BUFFER(R3)                                                    
         MVC   0(2,RF),=X'0D25'    TERMINATE RECORD WITH CRLF                   
         GOTO1 =V(PRNTBL),DMCB,=C'BUFFER DATA',,C'DUMP',(R2),=C'1D'             
*                                                                               
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         LA    R2,ATBSEND_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   SEND DATA                                    
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'EDICT01 *JDS/ENCODA HANDSH+        
               AKE WITH RECEIVER OK'                                            
*                                                                               
         MVC   PREP_TO_RECEIVE_TYPE,ATB_PREP_TO_RECEIVE_FLUSH                   
         LA    R2,ATBPTR_STRUCTURE                                              
         GOTO1 =A(CALLAPPC),(RC)   PREPARE TO RECEIVE                           
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    UNREGSTR            YES                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
         EJECT                                                                  
NEXTMSG  GOTO1 =A(GET1REC),(RC)    RECEIVE ONE RECORD FROM ENCODA               
         BNE   BADRCV              BAD RETURN CODE FROM APPC/MVS CALL           
*                                                                               
         CLC   =C'+++BIAS SEND',BUFFER                                          
         BNE   *+20                IGNORE THESE RECORDS                         
         XC    WORK2,WORK2                                                      
         XC    CUSTREF,CUSTREF                                                  
         B     CHKCONF                                                          
*                                                                               
         CLC   =C'0499',BUFFER     IS THIS A TIMESTAMP RECORD?                  
         BNE   *+14                NO -- UNKNOWN, SO THROW IT AWAY              
         MVC   WORK2,BUFFER        SAVE 0499 MESSAGE                            
         B     CHKCONF                                                          
*                                                                               
         CLC   =C'REF#=',BUFFER    IS THIS A TIMESTAMP RECORD?                  
         BNE   *+14                NO -- UNKNOWN, SO THROW IT AWAY              
         MVC   CUSTREF,BUFFER+5    SAVE REF #                                   
         B     CHKCONF                                                          
*                                                                               
CHKCONF  CLI   RQSTCONF,C'Y'       CONFIRMATION REQUESTED?                      
         BNE   GO_ON                                                            
*                                                                               
         LA    R2,ATBCFMD_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   ISSUE CONFIRMATION                           
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =A(WRTSTAMP),(RC)   YES -- WRITE IT TO EDICT FILE                
*                                                                               
GO_ON    CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    UNREGSTR                                                         
         B     NEXTMSG             RETURN CODE WAS OK                           
*                                                                               
BADSHAKE PRNT  BADHANDSHAKE,PRINT=ALWAYS                                        
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'EDICT01 *JDS/ENCODA BAD HA+        
               NDSHAKE WITH RECEIVER'                                           
         B     UNREGSTR                                                         
*                                                                               
BADRCV   PRNT  BADRECEIVE,PRINT=ALWAYS                                          
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'EDICT01 *JDS/ENCODA ERROR +        
               RECEIVING. TRANSMISSIONS ENDED.'                                 
*                                                                               
UNREGSTR OC    ALLOCATE_QUEUE_TOKEN,ALLOCATE_QUEUE_TOKEN                        
         BZ    GOODBYE             WE NEVER REGISTERED FOR ALLOCATES            
*                                                                               
         LA    R2,ATBURA2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   UNREGISTER FOR ALLOCATES                     
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GOODBYE  PRNT  EXITING,PRINT=ALWAYS                                             
*                                                                               
         XBASE                                                                  
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
INITIAL  NMOD1 0,INITIAL                                                        
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* INITIALIZE                                                                    
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(20,TODAY8)                                
         MVC   DELPERD+9(8),TODAY8 FOR PERVAL                                   
*                                                                               
         LA    R1,ENRSTOP                                                       
         STCM  R1,7,ASTOPECB+1     SET A(STOPPING ECB)                          
         L     RF,ENRMAJNM         A(MAJOR RESOURCE NAME)                       
         MVC   MAJORNAM,0(RF)      MAJOR RESOURCE NAME                          
         MVC   AXMTTBL,ENRXMTTB    A(TRANSMIT TABLE)                            
         MVC   DATAMGR,ENRADMGR    A(DATAMGR)                                   
*                                                                               
         L     RF,ENRLUNAM         A(LOCAL_LU_NAME)                             
         MVC   LOCAL_LU_NAME,0(RF)                                              
         L     RF,ENRTPNAM         A(TP_NAME)                                   
         MVC   TP_NAME,0(RF)                                                    
         MVI   TP_NAME+7,C'R'      SWITCH 'S' TO 'R'                            
*                                                                               
         BLDL  0,ENTRYPTS            BUILD LIST OF APPC/MVS ENTRY PTS           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                  BAD RETURN FROM BLDL MACRO                 
         LOAD  DE=ATBCFMD                                                       
         ST    R0,ATBCFMD_STRUCTURE                                             
         LOAD  DE=ATBEES3                                                       
         ST    R0,ATBEES3_STRUCTURE                                             
         LOAD  DE=ATBRAL2                                                       
         ST    R0,ATBRAL2_STRUCTURE                                             
         LOAD  DE=ATBRFA2                                                       
         ST    R0,ATBRFA2_STRUCTURE                                             
         LOAD  DE=ATBGTA2                                                       
         ST    R0,ATBGTA2_STRUCTURE                                             
         LOAD  DE=ATBPTR                                                        
         ST    R0,ATBPTR_STRUCTURE                                              
         LOAD  DE=ATBRCVW                                                       
         ST    R0,ATBRCVW_STRUCTURE                                             
         LOAD  DE=ATBSEND                                                       
         ST    R0,ATBSEND_STRUCTURE                                             
         LOAD  DE=ATBQAQ2                                                       
         ST    R0,ATBQAQ2_STRUCTURE                                             
         LOAD  DE=ATBURA2                                                       
         ST    R0,ATBURA2_STRUCTURE                                             
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* THIS ROUTINE WRITES AN INCOMING TIMESTAMP MESSAGE TO THE EDICT FILE           
*                                                                               
WRTSTAMP NMOD1 0,WRTSTAMP                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         PRNT  WRITETIMESTAMP,PRINT=ALWAYS                                      
*                                                                               
         OC    CUSTREF,CUSTREF                                                  
         BZ    WRTSTX                                                           
*                                                                               
         MVC   ACK,WORK2+94        GET THE ACK#                                 
         CLC   ACK,=C'01'          IGNORE SENT ACK#01 MESSAGE                   
         BE    WRTSTX                                                           
*                                                                               
         BRAS  RE,VCUSTREF                                                      
         BNE   WRTSTX                                                           
         BRAS  RE,UPDEDFIL                                                      
*                                                                               
WRTSTX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
VCUSTREF NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   CUSTREF+1(1),MAJORNAM+5   MATCH ON EDICT (ADV/REP)?              
         BNE   VCR10               NO -- DLN CAME IN OVER WRONG LINE            
*                                                                               
         CLI   CUSTREF,C'4'        VERSION 4 CUSTOMER REF. NUMBER?              
         BNE   VCR10                                                            
*                                                                               
         CLC   TODAY8,CUSTREF+10   NEED TO RESET TODAY'S DATE?                  
         BNL   VCR05               NO                                           
         GOTO1 =V(DATCON),DMCB,(5,0),(20,TODAY8)                                
         MVC   DELPERD+9(8),TODAY8 FOR PERVAL                                   
*                                                                               
VCR05    MVC   DELPERD(8),CUSTREF+10   DATE OF ORIGINAL SEND (YYYYMMDD)         
         GOTO1 =V(PERVAL),DMCB,(L'DELPERD,DELPERD),WORK                         
         CLI   DMCB+4,0                                                         
         BNE   VCR10               MAINFRAME DATE MUST BE SCREWED UP!           
         LA    RF,WORK                                                          
         CLC   PVALNDYS-PERVALD(2,RF),=AL2(28)                                  
         BNH   VCROK               DLN ISN'T MORE THAN 28 DAYS OLD              
*                                                                               
VCR10    MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(33),=C'INVALID CUSTOMER REFERENCE NUMBER'                   
         MVC   P+70(L'CUSTREF),CUSTREF                                          
         GOTO1 =V(PRINTER)                                                      
         B     VCRBAD              IGNORE THIS ONE                              
*                                                                               
VCROK    SR    RC,RC                                                            
VCRBAD   LTR   RC,RC                                                            
VCRNX    XIT1                                                                   
*                                                                               
UPDEDFIL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R1,DMCB                                                          
         USING CONVDSKD,R1                                                      
         MVC   CONVDMGR,DATAMGR    A(DATAMGR)                                   
         MVI   CONVACTN,CONVDSKO   CONVERT REFERENCE NUMBER TO DSKADDR          
         LA    RE,CUSTREF+2                                                     
         ST    RE,CONVOFF                                                       
         GOTO1 =V(CONVDSKA)                                                     
         MVC   FULL,CONVDSK                                                     
         DROP  R1                                                               
*                                                                               
         LA    R6,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R6)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(6),E,8)  ENQUEUE THE TRANSMIT TABLE                   
*                                                                               
         LA    R2,TABLNTRY         ASSUME WE'LL FIND REPORT IN TABLE            
         MVI   BYTE,C'T'           'T' FOR TABLE                                
*                                                                               
         L     R6,AXMTTBL          A(TRANSMIT TABLE)                            
         USING XMTTABLD,R3                                                      
         LA    R3,XMTENTRY                                                      
*                                                                               
UPEF50   MVC31 XMTENTRY,0(R6)                                                   
         CLI   0(R3),0             END OF TABLE?                                
         BNE   *+16                                                             
         L     R2,FULL             YES -- USE EDICT FILE DISK ADDRESS           
         MVI   BYTE,C'F'           'F' FOR FILE                                 
         B     UPEF60                                                           
         CLI   XMTSOURC,XMTSRCPQ   PRINT QUEUE ENTRY?                           
         BNE   *+14                                                             
         CLC   FULL,XMTDSKAD       FIND MATCH ON DISK ADDRESS                   
         BE    *+12                                                             
         AHI   R6,XMTTBLQ          BUMP TO NEXT ENTRY                           
         B     UPEF50                                                           
         MVC   TABLNTRY,0(R3)      SAVE XMIT TABLE ENTRY                        
         DROP  R3                                                               
*                                                                               
UPEF60   LA    R6,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R6)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(6),8)    DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
*                                                                               
         MVC   EXMTTBLE,AXMTTBL                                                 
         ST    R2,ETBLNTRY                                                      
*                                                                               
         CLC   ACK,=C'03'                                                       
         BNE   *+12                                                             
         MVI   EACTION,EACTDLVQ                                                 
         B     UPEF70                                                           
*                                                                               
         CLC   ACK,=C'02'                                                       
         BNE   UPEF90              INVALID MESSAGE                              
         MVI   EACTION2,EACTFWDQ                                                
*                                                                               
UPEF70   CLI   BYTE,C'F'           DISK ADDRESS BEING PASSED?                   
         BNE   *+8                                                              
         OI    EACTION,EACTFILQ    YES                                          
         MVC   EMAJORNM,=A(MAJORNAM)                                            
         MVC   EADTAMGR,DATAMGR                                                 
         GOTO1 =V(POSTSENT)                                                     
         BNE   UPEF80              COULD NOT POST EDICT FILE                    
         DROP  R1                                                               
*                                  EDICT FILE DISK ADDRESS                      
         GOTO1 =V(HEXOUT),DMCB,FULL,P+30,4,=C'TOG'                              
         PRNT  EDICTFILEPOSTED,PRINT=ALWAYS                                     
         B     UPEFX                                                            
*                                                                               
UPEF80   MVC   P+30(13),=C'*** ERROR ***'                                       
         PRNT  CANTPOST,PRINT=ALWAYS                                            
         B     UPEFX                                                            
*                                                                               
UPEF90   MVC   P+30(23),=CL23'*** INVALID MESSAGE ***'                          
         PRNT  PRINT=ALWAYS                                                     
*                                                                               
UPEFX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
GET1REC  NMOD1 0,GET1REC                                                        
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* RECEIVE A RECORD FROM ENCODA                                                  
*                                                                               
         MVI   RQSTCONF,C'N'       NO REQUEST FOR CONFIRMATION YET              
*                                                                               
         MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
         LA    R2,ATBRCVW_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   RECEIVE AND WAIT                             
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    GOODRCV                                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+10                BAD RETURN CODE                              
         LTR   RB,RB               SET CC NOT EQUAL                             
         B     BILINXIT                                                         
*                                                                               
         XC    ENRSTOP,ENRSTOP                                                  
*                                                                               
         CLC   STATUS_RECEIVED,ATB_CONFIRM_DEALLOC_RECEIVED                     
         BNE   GOT_ONE             PARTNER DID NOT ASK TO DEALLOCATE            
*                                                                               
         PRNT  GOTSTOPANDCONFIRM,PRINT=ALWAYS                                   
*                                                                               
         LA    R2,ATBCFMD_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   ISSUE CONFIRMATION                           
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    GOODRCV                                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                                                              
         DC    H'0'                WHY WOULD WE NOT BE ABLE TO CONFIRM?         
*                                                                               
         MVI   OPERSTOP,C'Y'       WE CAN STOP NOW                              
         B     GOODRCV                                                          
*                                                                               
GOT_ONE  CLI   TRACEFLG,C'Y'                                                    
         BNE   NOTRACE                                                          
         L     R2,RECEIVE_LENGTH                                                
         GOTO1 =V(PRNTBL),DMCB,0,BUFFER,C'DUMP',(R2),=C'1D'                     
*                                                                               
NOTRACE  CLC   STATUS_RECEIVED,ATB_CONFIRM_RECEIVED                             
         BNE   GOODRCV                                                          
         MVI   RQSTCONF,C'Y'                                                    
         PRNT  CONFIRMREQUESTED,PRINT=ALWAYS                                    
*                                                                               
GOODRCV  CR    RB,RB               SET CC EQUAL                                 
*                                                                               
BILINXIT XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
DATETIME NMOD1 0,DATETIME                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         TIME  DEC                                                              
         ST    R0,PRNTDUB                                                       
         MVI   PRNTDUB+4,X'0F'                                                  
         UNPK  PRNTTIME,PRNTDUB(5)                                              
         MVC   HHMMSS,PRNTTIME                                                  
         GOTO1 =V(DATCON),DMCB,(5,0),(20,YYYYMMDD)                              
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
PRNTATTB NMOD1 0,PRNTATTB                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PRINT DETAILS ABOUT THE CURRENT STATE OF THE LU6.2 CONVERSATION               
*                                                                               
         PRNT  *************,PRINT=ALWAYS                                       
         PRNT  GETATTRIBUTES,PRINT=ALWAYS                                       
         PRNT  *************,PRINT=ALWAYS                                       
*                                                                               
         LA    R2,ATBGTA2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   GET CONVERSATION ATTRIBUTES                  
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,CONVERSATION_ID,P+30,8,=C'TOG'                   
         PRNT  CONVERSATION_ID,PRINT=ALWAYS                                     
         MVC   P+30(17),PARTNER_LU_NAME                                         
         PRNT  PARTNER_LU_NAME,PRINT=ALWAYS                                     
         MVC   P+30(8),MODE_NAME                                                
         PRNT  MODE_NAME,PRINT=ALWAYS                                           
         MVC   P+30(4),=C'NONE'                                                 
         CLC   SYNC_LEVEL,ATB_NONE                                              
         BE    *+22                                                             
         MVC   P+30(7),=C'CONFIRM'                                              
         CLC   SYNC_LEVEL,ATB_CONFIRM                                           
         BE    *+6                                                              
         DC    H'0'                UNKNOWN SYNC LEVEL                           
         PRNT  SYNC_LEVEL,PRINT=ALWAYS                                          
         MVC   P+30(64),TP_NAME                                                 
         PRNT  TP_NAME,PRINT=ALWAYS                                             
         MVC   P+30(8),LOCAL_LU_NAME                                            
         PRNT  LOCAL_LU_NAME,PRINT=ALWAYS                                       
         MVC   P+30(5),=C'BASIC'                                                
         CLC   CONVERSATION_TYPE,ATB_BASIC_CONVERSATION                         
         BE    *+22                                                             
         MVC   P+30(6),=C'MAPPED'                                               
         CLC   CONVERSATION_TYPE,ATB_MAPPED_CONVERSATION                        
         BE    *+6                                                              
         DC    H'0'                UNKNOWN CONVERSATION TYPE                    
         PRNT  CONVERSATION_TYPE,PRINT=ALWAYS                                   
         MVC   P+30(4),=C'SEND'                                                 
         CLC   CONVERSATION_STATE,ATB_SEND_STATE                                
         BE    PRATTB10                                                         
         MVC   P+30(7),=C'RECEIVE'                                              
         CLC   CONVERSATION_STATE,ATB_RECEIVE_STATE                             
         BE    PRATTB10                                                         
         MVC   P+30(7),=C'CONFIRM'                                              
         CLC   CONVERSATION_STATE,ATB_CONFIRM_STATE                             
         BE    PRATTB10                                                         
         MVC   P+30(10),=C'INITIALIZE'                                          
         CLC   CONVERSATION_STATE,ATB_INITIALIZE_STATE                          
         BE    PRATTB10                                                         
         MVC   P+30(12),=C'SEND PENDING'                                        
         CLC   CONVERSATION_STATE,ATB_SEND_PENDING_STATE                        
         BE    PRATTB10                                                         
         MVC   P+30(12),=C'CONFIRM SEND'                                        
         CLC   CONVERSATION_STATE,ATB_CONFIRM_SEND_STATE                        
         BE    PRATTB10                                                         
         MVC   P+30(18),=C'CONFIRM DEALLOCATE'                                  
         CLC   CONVERSATION_STATE,ATB_CONFIRM_DEALLOCATE_STATE                  
         BE    PRATTB10                                                         
         DC    H'0'                UNKNOWN CONVERSATION STATE                   
PRATTB10 PRNT  CONVERSATION_STATE,PRINT=ALWAYS                                  
*                                                                               
         PRNT  *************,PRINT=ALWAYS                                       
         PRNT  *************,PRINT=ALWAYS                                       
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
QUERYALQ NMOD1 0,QUERYALQ                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PRINT DETAILS ABOUT THE CURRENT STATE OF THE ALLOCATE QUEUE                   
*                                                                               
         PRNT  **************,PRINT=ALWAYS                                      
         PRNT  ALLOCATE_QUEUE,PRINT=ALWAYS                                      
         PRNT  **************,PRINT=ALWAYS                                      
*                                                                               
         LA    R2,ATBQAQ2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   QUERY ALLOCATE QUEUE                         
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,ALLOCATE_QUEUE_TOKEN,P+30,8,=C'TOG'              
         PRNT  ALLOCATE_Q_TOKEN,PRINT=ALWAYS                                    
         EDIT  TP_NAME_LENGTH,(5,P+30),ALIGN=LEFT,ZERO=NOBLANK                  
         PRNT  TP_NAME_LENGTH,PRINT=ALWAYS                                      
         MVC   P+30(64),TP_NAME                                                 
         PRNT  TP_NAME,PRINT=ALWAYS                                             
         MVC   P+30(8),LOCAL_LU_NAME                                            
         PRNT  LOCAL_LU_NAME,PRINT=ALWAYS                                       
         EDIT  ALLOCATE_QUEUE_SIZE,(5,P+30),ALIGN=LEFT,ZERO=NOBLANK             
         PRNT  ALLOCATE_Q_SIZE,PRINT=ALWAYS                                     
         EDIT  ALLOCATE_QUEUE_OLDEST,(5,P+30),ALIGN=LEFT,ZERO=NOBLANK           
         PRNT  ALLOCATE_Q_AGE,PRINT=ALWAYS                                      
         OC    LAST_REC_ALLOC_ISSUED,LAST_REC_ALLOC_ISSUED                      
         BNZ   *+14                                                             
         MVC   P+30(4),=C'NONE'                                                 
         B     QUALC10                                                          
         GOTO1 =V(HEXOUT),DMCB,LAST_REC_ALLOC_ISSUED,P+30,8,=C'TOG'             
QUALC10  PRNT  LAST_ALLOC_ISSUED,PRINT=ALWAYS                                   
         OC    LAST_REC_ALLOC_RETURNED,LAST_REC_ALLOC_RETURNED                  
         BNZ   *+14                                                             
         MVC   P+30(4),=C'NONE'                                                 
         B     QUALC20                                                          
         GOTO1 =V(HEXOUT),DMCB,LAST_REC_ALLOC_RETURNED,P+30,8,=C'TOG'           
QUALC20  PRNT  LAST_ALLOC_RETURN,PRINT=ALWAYS                                   
*                                                                               
         PRNT  *************,PRINT=ALWAYS                                       
         PRNT  *************,PRINT=ALWAYS                                       
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
CALLAPPC NMOD1 0,CALLAPPC                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* UPON ENTRY,  R2 POINTS TO PARAMETER STRUCTURE                                 
* UPON RETURN, RETURN_CODE CONTAINS THE APPC/MVS RETURN CODE                    
*                                                                               
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_ECB                                  
         TM    20(R2),X'80'                                                     
         BZ    *+10                                                             
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         L     RF,0(R2)            RF = A(APPC/MVS ROUTINE)                     
         LA    R3,24(R2)           R3 = A(PARAMETER LIST)                       
         CALL  (15),MF=(E,(R3))                                                 
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         TM    20(R2),X'80'        ASYNCHRONOUS CALL?                           
         BZ    CALL10              YES, SO WE MUST WAIT                         
*                                                                               
         L     RF,ASTOPECB         SYNCHRONOUS CALL, SO IT'S DONE               
         TM    0(RF),X'40'         STOP?                                        
         BZ    CALL30              NO                                           
         XC    0(4,RF),0(RF)                                                    
         MVI   OPERSTOP,C'Y'                                                    
         B     CALL30                                                           
*                                                                               
CALL10   CLC   RETURN_CODE,ATB_OK                                               
         BE    CALL20              APPC/MVS CALL WAS ACCEPTED                   
         MVC   P+30(16),4(R2)      PRINT NAME OF APPC/MVS ROUTINE               
         MVC   P+50(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+64),ALIGN=LEFT                                  
         PRNT  **BAD_APPC_CALL**,PRINT=ALWAYS                                   
         DC    H'0'                APPC/MVS CALL NOT ACCEPTED                   
*                                                                               
CALL20   WAIT  1,ECBLIST=ECBLST    WAIT FOR COMPLETION OR OPERATOR              
*                                                                               
         L     RF,ASTOPECB                                                      
         TM    0(RF),X'40'         STOP?                                        
         BZ    *+18                NO                                           
         XC    0(4,RF),0(RF)                                                    
         MVI   OPERSTOP,C'Y'                                                    
         B     CALL30                                                           
*                                                                               
         TM    APPCECB,X'40'                                                    
         BO    *+6                                                              
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
*                                                                               
         MVC   RETURN_CODE,APPCECB                                              
         MVI   RETURN_CODE,0       CLEAR HIGH-ORDER BYTE                        
         XC    APPCECB,APPCECB                                                  
*                                                                               
CALL30   MVI   P,C'*'              '*' MEANS IT'S AN APPC/MVS CALL              
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODE           
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         PRNT  PRINT=ALWAYS                                                     
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    CALLX                                                            
         TM    20(R2),X'40'        CAN CALL ERROR_EXTRACT NOW?                  
         BZ    CALLX                                                            
         GOTO1 =A(APPCERR),(RC)    YES - DO IT                                  
*                                                                               
CALLX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
APPCERR  NMOD1 0,APPCERR                                                        
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PERFORM APPC ERROR_EXTRACT FUNCTION.                                          
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         LA    R2,ATBEES3_STRUCTURE                                             
         L     RF,0(R2)            RF = A(APPC/MVS ROUTINE)                     
         LA    R3,24(R2)           R3 = A(PARAMETER LIST)                       
         CALL  (15),MF=(E,(R3))                                                 
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         MVI   P,C'*'              '*' MEANS IT'S AN APPC/MVS CALL              
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODE           
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE_ERROR_EXTRACT,(5,P+44),ALIGN=LEFT,          +        
               ZERO=NOBLANK                                                     
         PRNT  PRINT=ALWAYS                                                     
*                                                                               
         OC    RETURN_CODE_ERROR_EXTRACT,RETURN_CODE_ERROR_EXTRACT              
         BNZ   APPCEX              BAD RETURN CODE, SO NO ERROR INFO            
*                                                                               
         MVC   P+30(37),=C'APPC SERVICE XXXXXXXX, REASON_CODE = '               
         MVC   P+43(8),SERVICE_NAME                                             
         EDIT  SERVICE_REASON_CODE,(5,P+67),ALIGN=LEFT,ZERO=NOBLANK             
         PRNT  ERROR_EXTRACT,PRINT=ALWAYS                                       
*                                                                               
         ICM   R3,15,MESSAGE_TEXT_LENGTH                                        
         BZ    APPCE20             NOTHING IN FIELD                             
         C     R3,=F'132'          MESSAGE > 132 CHARACTERS?                    
         BH    APPCE10             YES, BREAK MESSAGE UP INTO TWO LINES         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),MESSAGE_TEXT                                                
         GOTO1 =V(PRINTER)                                                      
         B     APPCE20                                                          
*                                                                               
APPCE10  S     R3,=F'132'                                                       
         MVC   P,MESSAGE_TEXT                                                   
         GOTO1 =V(PRINTER)                                                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),MESSAGE_TEXT+132                                            
         GOTO1 =V(PRINTER)                                                      
*                                                                               
APPCE20  ICM   R3,15,ERROR_LOG_INFORMATION_LENGTH                               
         BZ    APPCEX              NOTHING IN FIELD                             
         C     R3,=F'132'          MESSAGE > 132 CHARACTERS?                    
         BH    APPCE30             YES, BREAK MESSAGE UP                        
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),ERROR_LOG_INFORMATION                                       
         GOTO1 =V(PRINTER)                                                      
         B     APPCEX                                                           
*                                                                               
APPCE30  C     R3,=F'264'          WILL MESSAGE FIT ON TWO LINES?               
         BH    APPCE40                                                          
         S     R3,=F'132'          YES                                          
         MVC   P,ERROR_LOG_INFORMATION                                          
         GOTO1 =V(PRINTER)                                                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),ERROR_LOG_INFORMATION+132                                   
         GOTO1 =V(PRINTER)                                                      
         B     APPCEX                                                           
*                                                                               
APPCE40  C     R3,=F'396'          WILL MESSAGE FIT ON THREE LINES?             
         BH    APPCE50                                                          
         S     R3,=F'264'          YES                                          
         MVC   P,ERROR_LOG_INFORMATION                                          
         GOTO1 =V(PRINTER)                                                      
         MVC   P,ERROR_LOG_INFORMATION+132                                      
         GOTO1 =V(PRINTER)                                                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),ERROR_LOG_INFORMATION+264                                   
         GOTO1 =V(PRINTER)                                                      
         B     APPCEX                                                           
*                                                                               
APPCE50  S     R3,=F'396'          SPLIT INTO FOUR LINES                        
         MVC   P,ERROR_LOG_INFORMATION                                          
         GOTO1 =V(PRINTER)                                                      
         MVC   P,ERROR_LOG_INFORMATION+132                                      
         GOTO1 =V(PRINTER)                                                      
         MVC   P,ERROR_LOG_INFORMATION+264                                      
         GOTO1 =V(PRINTER)                                                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),ERROR_LOG_INFORMATION+396                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
APPCEX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
COMMWORK DS    0D                  COMMON STORAGE AREA                          
         SPACE 2                                                                
         ATBSERV                                                                
         SPACE 2                                                                
         ATBCTASM                                                               
         SPACE 2                                                                
* APPC/MVS CALL PARAMETERS                                                      
*                                                                               
CONVERSATION_TYPE              DS    F                                          
SYM_DEST_NAME                  DC    CL8' '                                     
PARTNER_LU_NAME                DC    CL17' '                                    
LOCAL_LU_NAME                  DC    CL8' '                                     
MODE_NAME                      DS    CL8                                        
TP_NAME_LENGTH                 DC    F'8'                                       
TP_NAME                        DC    CL64' '                                    
SYNC_LEVEL                     DS    F                                          
USER_ID                        DC    CL10' '                                    
PROFILE                        DC    CL10' '                                    
LUW_ID                         DS    XL26                                       
ALLOCATE_QUEUE_SIZE            DS    F                                          
ALLOCATE_QUEUE_OLDEST          DS    F                                          
LAST_REC_ALLOC_ISSUED          DS    CL8                                        
LAST_REC_ALLOC_RETURNED        DS    CL8                                        
CONVERSATION_ID                DS    CL8                                        
CONVERSATION_CORRELATOR        DS    CL8                                        
ALLOCATE_QUEUE_TOKEN           DC    CL8'00'                                    
NOTIFY_TYPE                    DS    F                                          
                               DC    A(APPCECB) MUST FOLLOW NOTIFY_TYPE         
RETURN_CODE                    DS    F                                          
REASON_CODE                    DS    F                                          
FILL                           DC    F'0'   ATB_FILL_LL                         
CONVERSATION_STATE             DS    F                                          
RECEIVE_LENGTH                 DS    F                                          
RECEIVE_ACCESS_TOKEN           DC    F'0'                                       
STATUS_RECEIVED                DS    F                                          
DATA_RECEIVED                  DS    F                                          
REQUEST_TO_SEND_RECEIVED       DS    F                                          
REQUEST_TO_SEND_VALUE          DS    F                                          
SEND_TYPE                      DS    F                                          
SEND_LENGTH                    DS    F                                          
SEND_ACCESS_TOKEN              DC    F'0'                                       
***RECEIVE_ALLOCATE_TYPE          DC    F'3'   ATBCTS_TIMED                     
***TIME_OUT_VALUE                 DC    F'600' TEN MINUTES                      
RECEIVE_ALLOCATE_TYPE          DC    F'2'   ATBCTS_WAIT                         
TIME_OUT_VALUE                 DC    F'0'   NO TIMEOUT FOR NOW                  
PREP_TO_RECEIVE_TYPE           DS    F                                          
LOCKS                          DS    F                                          
BUFFER_LENGTH                  DS    H      (BUFFER MUST FOLLOW THIS)           
BUFFER                         DS    CL256                                      
SERVICE_NAME                   DS    CL8                                        
SERVICE_REASON_CODE            DS    F                                          
MESSAGE_TEXT_LENGTH            DS    F                                          
MESSAGE_TEXT                   DS    CL256                                      
ERROR_LOG_PRODUCT_SET_ID_LENGTH DS   F                                          
ERROR_LOG_PRODUCT_SET_ID       DS    CL256                                      
ERROR_LOG_INFORMATION_LENGTH   DS    F                                          
ERROR_LOG_INFORMATION          DS    CL512                                      
REASON_CODE_ERROR_EXTRACT      DS    F                                          
RETURN_CODE_ERROR_EXTRACT      DS    F                                          
         EJECT                                                                  
CRLF     DS    0XL2                                                             
CR       DC    X'0D'                                                            
LF       DC    X'25'                                                            
         SPACE 2                                                                
APPCM1   DC    C'+++BIAS SENDER='                                               
APPCM1Q  EQU   *-APPCM1                                                         
         DC    C'DDS '                                                          
APPCM1A  DC    C'CONV=SEND VERSION=1 '                                          
APPCM1AQ EQU   *-APPCM1A                                                        
         DC    C'DATE=YYYYMMDD TIME=HHMMSS'                                     
         SPACE 2                                                                
APPCM2   DC    C'+++BIAS SENDER=DDS VERSION=1 STATUS=READY '                    
         DC    C'DATE=YYYYMMDD TIME=HHMMSS'                                     
APPCM2AQ EQU   *-APPCM2                                                         
         SPACE 2                                                                
DMCB     DS    10F                                                              
DUB      DS    D                                                                
PRNTDUB  DS    D                   FOR PRNT MACRO                               
EDITDUB  DS    D                   FOR EDIT MACRO                               
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
PRNTTIME DS    CL9                 FOR PRNT MACRO                               
EDITWRK  DS    CL17                FOR EDIT MACRO                               
YYYYMMDD DS    CL8                 DATE                                         
HHMMSS   DS    CL6                 TIME                                         
EDICTREC DS    XL256               FOR BUILDING EDICT FILE RECORD               
TODAY8   DS    CL8                 EBCDIC DATE TODAY (YYYYMMDD)                 
DELPERD  DC    C'YYYYMMDD-YYYYMMDD'  FOR PERVAL: SEND DATE - TODAY              
ACK      DS    CL2                 01, 02, 03                                   
CUSTREF  DS    CL18                VEDDNNNNNNYYYYMMDD                           
WORK     DS    CL256                                                            
WORK2    DS    CL256                                                            
         SPACE 2                                                                
ECBLST   DS    0F                                                               
ASTOPECB DC    X'00',AL3(0)        A(STOPECB)                                   
AAPPCECB DC    X'80',AL3(APPCECB)  A(APPCECB)                                   
         SPACE 2                                                                
MAJORNAM DS    CL8                 MAJOR RESOURCE NAME                          
DATAMGR  DS    A                   A(DATAMGR)                                   
AXMTTBL  DS    A                   A(XMIT REPORTS TABLE)                        
XMTENTRY DS    XL(XMTTBLQ)         TEMP STORAGE FOR 1 XIMTABLE ENTRY            
TABLNTRY DS    XL(XMTTBLQ)         SAVED XMIT TABLE KEY                         
APPCECB  DC    F'0'                ECB FOR APPC/MVS CALLS                       
OPERSTOP DC    C'N'                'Y' = OPERATOR ENTERED 'STOP'                
TRACEFLG DS    C                   'Y' = PRINT DETAILED TRACE                   
RQSTCONF DS    C                   'Y' = WE GOT REQUEST FOR CONFIRM             
EDICTDA  DS    CL8                 EDICT FILE DISK ADDRESS                      
         DS    XL4                 PQ RECORD LENGTH                             
R        DS    CL256               PQ RECORD DATA                               
         EJECT                                                                  
* PARAMETER LISTS FOR APPC/MVS CALLS                                            
*                                                                               
*  F    A(ROUTINE)                                                              
*  CL16 EBCDIC ROUTINE NAME                                                     
*  XL1  FLAGS                                                                   
*       X'80': ROUTINE IS SYNCHRONOUS ONLY                                      
*       X'40': ROUTINE CAN BE FOLLOWED BY EXTRACT_ERROR CALL                    
*  XL3  SPARE                                                                   
*  PARAMETERS (STANDARD IBM FORMAT)                                             
*                                                                               
ATBRAL2_STRUCTURE         DS    A                                               
                          DC    CL16'RECEIVE_ALLOCATE'                          
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'00',AL3(ALLOCATE_QUEUE_TOKEN)                 
                          DC    X'00',AL3(RECEIVE_ALLOCATE_TYPE)                
                          DC    X'00',AL3(TIME_OUT_VALUE)                       
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(CONVERSATION_TYPE)                    
                          DC    X'00',AL3(PARTNER_LU_NAME)                      
                          DC    X'00',AL3(MODE_NAME)                            
                          DC    X'00',AL3(SYNC_LEVEL)                           
                          DC    X'00',AL3(USER_ID)                              
                          DC    X'00',AL3(PROFILE)                              
                          DC    X'00',AL3(REASON_CODE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBRFA2_STRUCTURE         DS    A                                               
                          DC    CL16'REGISTR_4_ALLOCS'                          
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'00',AL3(SYM_DEST_NAME)                        
                          DC    X'00',AL3(TP_NAME_LENGTH)                       
                          DC    X'00',AL3(TP_NAME)                              
                          DC    X'00',AL3(LOCAL_LU_NAME)                        
                          DC    X'00',AL3(PARTNER_LU_NAME)                      
                          DC    X'00',AL3(USER_ID)                              
                          DC    X'00',AL3(PROFILE)                              
                          DC    X'00',AL3(ALLOCATE_QUEUE_TOKEN)                 
                          DC    X'00',AL3(REASON_CODE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBURA2_STRUCTURE         DS    A                                               
                          DC    CL16'UNREGISTR_4_ALLC'                          
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'00',AL3(ALLOCATE_QUEUE_TOKEN)                 
                          DC    X'00',AL3(REASON_CODE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBPTR_STRUCTURE          DS    A                                               
                          DC    CL16'PREP_TO_RECEIVE'                           
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(PREP_TO_RECEIVE_TYPE)                 
                          DC    X'00',AL3(LOCKS)                                
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBRCVW_STRUCTURE         DS    A                                               
                          DC    CL16'RECEIVE_AND_WAIT'                          
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(FILL)                                 
                          DC    X'00',AL3(RECEIVE_LENGTH)                       
                          DC    X'00',AL3(RECEIVE_ACCESS_TOKEN)                 
                          DC    X'00',AL3(BUFFER_LENGTH)                        
                          DC    X'00',AL3(STATUS_RECEIVED)                      
                          DC    X'00',AL3(DATA_RECEIVED)                        
                          DC    X'00',AL3(REQUEST_TO_SEND_RECEIVED)             
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBSEND_STRUCTURE         DS    A                                               
                          DC    CL16'SEND_DATA'                                 
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(SEND_TYPE)                            
                          DC    X'00',AL3(SEND_LENGTH)                          
                          DC    X'00',AL3(SEND_ACCESS_TOKEN)                    
                          DC    X'00',AL3(BUFFER_LENGTH)                        
                          DC    X'00',AL3(REQUEST_TO_SEND_VALUE)                
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBCFMD_STRUCTURE         DS    A                                               
                          DC    CL16'ISSUE_CONFIRM'                             
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBGTA2_STRUCTURE         DS    A                                               
                          DC    CL16'GET_ATTRIBUTES'                            
                          DC    X'C0'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(PARTNER_LU_NAME)                      
                          DC    X'00',AL3(MODE_NAME)                            
                          DC    X'00',AL3(SYNC_LEVEL)                           
                          DC    X'00',AL3(CONVERSATION_CORRELATOR)              
                          DC    X'00',AL3(LUW_ID)                               
                          DC    X'00',AL3(WORK)                                 
                          DC    X'00',AL3(TP_NAME)                              
                          DC    X'00',AL3(LOCAL_LU_NAME)                        
                          DC    X'00',AL3(CONVERSATION_TYPE)                    
                          DC    X'00',AL3(WORK)                                 
                          DC    X'00',AL3(WORK)                                 
                          DC    X'00',AL3(WORK)                                 
                          DC    X'00',AL3(CONVERSATION_STATE)                   
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBQAQ2_STRUCTURE         DS    A                                               
                          DC    CL16'QUERY_ALLOCATE_Q'                          
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'00',AL3(ALLOCATE_QUEUE_TOKEN)                 
                          DC    X'00',AL3(TP_NAME_LENGTH)                       
                          DC    X'00',AL3(TP_NAME)                              
                          DC    X'00',AL3(LOCAL_LU_NAME)                        
                          DC    X'00',AL3(ALLOCATE_QUEUE_SIZE)                  
                          DC    X'00',AL3(ALLOCATE_QUEUE_OLDEST)                
                          DC    X'00',AL3(LAST_REC_ALLOC_ISSUED)                
                          DC    X'00',AL3(LAST_REC_ALLOC_RETURNED)              
                          DC    X'00',AL3(REASON_CODE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBEES3_STRUCTURE         DS    A                                               
                          DC    CL16'ERROR_EXTRACT'                             
                          DC    X'80'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(SERVICE_NAME)                         
                          DC    X'00',AL3(SERVICE_REASON_CODE)                  
                          DC    X'00',AL3(MESSAGE_TEXT_LENGTH)                  
                          DC    X'00',AL3(MESSAGE_TEXT)                         
                          DC X'00',AL3(ERROR_LOG_PRODUCT_SET_ID_LENGTH)         
                          DC    X'00',AL3(ERROR_LOG_PRODUCT_SET_ID)             
                          DC    X'00',AL3(ERROR_LOG_INFORMATION_LENGTH)         
                          DC    X'00',AL3(ERROR_LOG_INFORMATION)                
                          DC    X'00',AL3(REASON_CODE_ERROR_EXTRACT)            
                          DC    X'80',AL3(RETURN_CODE_ERROR_EXTRACT)            
         SPACE 3                                                                
ENTRYPTS DS    0F                                                               
         DC    Y((ENTRYLSQ-ENTRYSTQ)/60)   NUMBER OF TABLE ENTRIES              
         DC    H'60'                       MUST REMAIN AS 60                    
ENTRYSTQ EQU   *                                                                
*                                                                               
ATBCFMD  DC    CL8'ATBCFMD'                                                     
         DC    XL52'00'                                                         
ATBEES3  DC    CL8'ATBEES3'                                                     
         DC    XL52'00'                                                         
ATBGTA2  DC    CL8'ATBGTA2'                                                     
         DC    XL52'00'                                                         
ATBPTR   DC    CL8'ATBPTR'                                                      
         DC    XL52'00'                                                         
ATBQAQ2  DC    CL8'ATBQAQ2'                                                     
         DC    XL52'00'                                                         
ATBRAL2  DC    CL8'ATBRAL2'                                                     
         DC    XL52'00'                                                         
ATBRCVW  DC    CL8'ATBRCVW'                                                     
         DC    XL52'00'                                                         
ATBRFA2  DC    CL8'ATBRFA2'                                                     
         DC    XL52'00'                                                         
ATBSEND  DC    CL8'ATBSEND'                                                     
         DC    XL52'00'                                                         
ATBURA2  DC    CL8'ATBURA2'                                                     
         DC    XL52'00'                                                         
*                                                                               
ENTRYLSQ EQU   *                                                                
         EJECT                                                                  
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
         SPACE 3                                                                
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
       ++INCLUDE DDEDICTWRK                                                     
         EJECT                                                                  
       ++INCLUDE DDEDICTFIL                                                     
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         EJECT                                                                  
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014DDEDIENCR 01/12/06'                                      
         END                                                                    
