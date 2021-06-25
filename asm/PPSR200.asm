*          DATA SET PPSR200    AT LEVEL 011 AS OF 01/28/14                      
*PHASE T42100A                                                                  
*INCLUDE PPBYOUT                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T42100- ENHANCED SPACE RESERVATION BASE'                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 09/27/13 USE RUN COMMAND TO HANDLE EMBEDDED COMMENTS                     
*                                                                               
* KWAN 06/28/05 ENHANCED SPACE RESERVATION KICKOFF                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T42100   CSECT                                                                  
         LKSVR TYPE=UR,BLOCKS=(LIOBSB2Q,T421FFD)                                
*                                                                               
         PRINT NOGEN                                                            
         NMOD1 PPSR200X-PPSR200D,T42100,RR=R6,CLEAR=YES                         
*                                                                               
         USING POLWRKD,RC                                                       
         USING T421FFD,RA                                                       
         LR    R8,RC                                                            
         A     R8,=A(POLWRKX-POLWRKD)                                           
         USING ESRWORKD,R8                                                      
         LR    R9,R8                                                            
         A     R9,=A(ESRWORKX-ESRWORKD)                                         
         LR    R7,R9                                                            
         AHI   R7,4096                                                          
         USING POLFILED,R9,R7                                                   
*                                                                               
         LM    R2,R5,0(R1)                                                      
         ST    R5,VTIA                                                          
         PACK  AGYNUM,0(1,R1)      AGENCY NUMBER                                
         LH    R5,0(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,FRSTFLD          A(FIRST INPUT FIELD HEADER)                  
         LH    R5,2(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,LASTFLD          A(LAST INPUT FIELD)                          
         MVC   NUMFLD,4(R2)        NUMBER OF FIELDS                             
         ST    R3,VTWA             A(TWA)                                       
         MVC   VDATAMGR(36),0(R4)  FACILITY LIST                                
         LR    RA,R3                                                            
         MVC   TERMNAL,0(RA)       TERMINAL NUMBER                              
         MVC   AGYALPHA,14(RA)     ALPHA AGENCY CODE                            
         LA    R3,64(R3)                                                        
         ST    R3,ERRAREA          PRESET ERRAREA TO A(FIRST HEADER)            
         MVI   DMINBTS,X'C0'       PRESET DATAMGR CONTROL BITS                  
         MVI   DMOUTBTS,X'FD'      PRESET DATAMGR ERROR CHECK BITS              
         MVC   DATCON,36(R4)       EXPANDED FACILITIES LIST                     
         MVC   VCOMFACS,16(R1)                                                  
*                                                                               
         ST    R6,RELO00                                                        
         BRAS  RE,INITWKST         INITIALIZE WORKING STORAGE AREAS             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         MVI   ADBSW,0             INIT ADBUYER SWITCH                          
         BRAS  RE,CKADBYER         ANY GLOBBER CALLS?                           
         BNE   SR2_10                                                           
*                                                                               
         BRAS  RE,LKIO_GET         LINKIO WILL SET FLDS                         
*                                                                               
         BRAS  RE,S#TB_INI         SERIAL# TABLE INITIALIZATION                 
*                                                                               
         BRAS  RE,LKIO_GET         PREPARE FOR MULTIPLE REPLY RECORDS           
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SR2_10   XC    SVERRFLD,SVERRFLD   CLEAR ERROR FIELD NUMBER (FOR ADB)           
*                                                                               
         GOTOR VCALLOV,DMCB,(X'01',0),(RA)                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTOR (RF),(R1),(RC),(RA)                                              
         CLI   ERRAREA,0           ERROR ENCOUNTERED?                           
         BNE   EXXMOD                                                           
*                                                                               
         TM    ADBSW,AS_ESRUQ      ESR UPLOAD?                                  
         BZ    SR2_20                                                           
         GOTOR VT42105,DMCB,(RC),(RA),('PRCESR#Q',0)                            
*                                                                               
SR2_20   XC    KEY,KEY                                                          
*                                                                               
SR2_30   GOTOR VCALLOV,DMCB,(X'02',0),(RA)                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTOR (RF),(R1),(RC),(RA)                                              
         OC    ERR(2),ERR          ERROR ENCOUNTERED?                           
         BZ    SR2_40                                                           
*                                                                               
         OI    PRQUIDH+6,X'40'     INSERT CURSOR                                
         SR    R0,R0                                                            
         ICM   R0,3,ERR            ERROR NUMBER                                 
         BRAS  RE,GET_ETXT                                                      
         B     EXXMOD                                                           
*                                                                               
SR2_40   CLI   PBUYKEY,X'FF'                                                    
         BNE   SR2_30              GO READ MORE BUYS                            
*                                                                               
SR2_80   DS    0H                  BUY DISK ADDRESSES ARE SET                   
*                                                                               
         OC    INSCNT,INSCNT                                                    
         BZ    SR2_88                                                           
*                                                                               
         GOTOR VCALLOV,DMCB,(X'10',0),(RA)                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTOR (RF),(R1),(RC),(RA),('RPYESRHQ',0)                               
*                                                                               
         GOTOR VCALLOV,DMCB,(X'11',0),(RA)                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTOR (RF),(R1),(RC),(RA)                                              
*                                                                               
         MVC   WORK(L'SVPSCOM1),SVPSCOM1                                        
         LHI   RE,E#COMVAR                                                      
         STH   RE,HALF                                                          
         BRAS  RE,GTSTDCMR                                                      
*                                                                               
         MVC   WORK(L'SVPSCOM2),SVPSCOM2                                        
         LHI   RE,E#COMVAR                                                      
         STH   RE,HALF                                                          
         BRAS  RE,GTSTDCMR                                                      
*                                                                               
         MVC   WORK(L'SVMCSCMC),SVMCSCMC                                        
         LHI   RE,E#STDCOM                                                      
         STH   RE,HALF                                                          
         BRAS  RE,GTSTDCMR                                                      
*                                                                               
         LA    R2,PCLTELEM                                                      
         MVI   ELCODE,X'10'        CONTRACT STANDARD COMM ELEM                  
         BRAS  RE,NXTEL                                                         
         BNE   SR2_82                                                           
         MVC   WORK(L'PCLTCNUM),2(R2)                                           
         LHI   RE,E#STDCOM                                                      
         STH   RE,HALF                                                          
         BRAS  RE,GTSTDCMR                                                      
*                                                                               
SR2_82   CLI   PCLTPROF+5,C'2'     SUB CLIENT?                                  
         BNE   SR2_84                                                           
         MVC   WORK(L'PCLTKCLT),PCLTPROF+6                                      
         MVC   WORK+L'PCLTKCLT(L'SVMCOFFC),SVMCOFFC                             
         LHI   RE,E#STDCOM                                                      
         STH   RE,HALF                                                          
         BRAS  RE,GTCONCMR                                                      
*                                                                               
SR2_84   MVC   WORK(L'PCLTKCLT),PCLTKCLT                                        
         MVC   WORK+L'PCLTKCLT(L'PCLTOFF),PCLTOFF                               
         LHI   RE,E#STDCOM                                                      
         STH   RE,HALF                                                          
         BRAS  RE,GTCONCMR                                                      
*                                                                               
         GOTOR VCALLOV,DMCB,(X'10',0),(RA)                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTOR (RF),(R1),(RC),(RA),('MRKBUYRQ',0)                               
*                                                                               
SR2_88   EDIT  INSCNT,(4,PRQLN1)                                                
         OI    PRQLN1+3,X'F0'                                                   
*                                                                               
         MVC   PRQLN1+5(10),=C'INSERTIONS'                                      
         CHI   R0,1                                                             
         BNE   *+8                                                              
         MVI   PRQLN1+14,C' '                                                   
         OI    PRQLN1H+6,X'80'                                                  
*                                                                               
         EDIT  IOCNT,(4,PRQLN2)                                                 
         OI    PRQLN2+3,X'F0'                                                   
         MVC   PRQLN2+5(11),=C'INS. ORDERS'                                     
         CHI   R0,1                                                             
         BNE   *+8                                                              
         MVI   PRQLN2+15,C' '                                                   
         OI    PRQLN2H+6,X'80'                                                  
*                                                                               
         XC    PRQLN3,PRQLN3                                                    
         OI    PRQLN3H+6,X'80'                                                  
*                                                                               
         LHI   R0,PRCDENXR         REQUEST PROCESSED, ENTER NEXT REQ            
         BRAS  RE,GET_ITXT                                                      
         OI    PRQMSGH+6,X'80'                                                  
         LA    R2,PRQMEDH                                                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TM    ADBSW,AS_ADBRQ      ADBUYER REQUEST?                             
         JZ    EXXMODX             NO                                           
*                                                                               
         L     R3,AWRKREC          NEED TO BUILD ADBUYER REPLY DATA             
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
*                                                                               
         BRAS  RE,RPYTRAIL         REPLY ESR TRAILER DATA                       
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#SR2FIN)              
*                                                                               
SR2_110  GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#TOTINS),    +        
               ('LD_UBINQ',NUMPRCIN),(L'NUMPRCIN,0)                             
*                                                                               
         L     R4,ASER#TAB         POINT TO SERIAL# TABLE                       
         USING SER#TABD,R4                                                      
         SR    R2,R2                                                            
         ICM   R2,3,NUMSER#S       NUMBER OF SERIAL#S IN TABLE                  
SR2_116  CLI   S#STATUS,S#NOTU_Q   SERIAL# NOT USED IN INSERTION ORDER?         
         BNE   SR2_116H                                                         
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#BYSER),     +        
               ('LD_SPAKQ',S#SERIAL),(L'S#SERIAL,0)                             
SR2_116H LA    R4,SER#TBLQ(R4)     POINT TO NEXT ENTRY IN TABLE                 
         BCT   R2,SR2_116                                                       
*                                                                               
         TM    ADBSW,AS_ESRUQ      ESR UPLOAD?                                  
         JZ    ALLDONE             NO                                           
         OC    NUMPRCIN,NUMPRCIN   ANY INSERTION ORDER PROCESSED?               
         JZ    ALLDONE                                                          
         GOTOR VT42105,DMCB,(RC),(RA),('PUTESR#Q',0)                            
         J     ALLDONE                                                          
*                                                                               
         DROP  R3,R4                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GET_ITXT ST    RE,FULL             SAVE RETURN ADDRESS                          
         XC    PRQMSG,PRQMSG                                                    
         L     RF,VCOMFACS                                                      
         L     RF,(CGETTXT-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB+12,(R0),0,(C'I',DMCB),0,0,0                            
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
GET_ETXT ST    RE,FULL             SAVE RETURN ADDRESS                          
         XC    PRQMSG,PRQMSG                                                    
         L     RF,VCOMFACS                                                      
         L     RF,(CGETTXT-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB+12,(R0),0,(C'E',DMCB),0,0,0                            
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
LKIO_GET ST    RE,FULL                                                          
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         GOTOR ALINKIO,DMCB,('LIOAGET',LIOBD)                                   
         L     RE,FULL                                                          
         BR    RE                                                               
         DROP  R3                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* COMMUNICATION WITH DATA MANAGER (DIRECTORY)                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VREAD    LA    RF,DMREAD                                                        
         MVC   KEYSAVE,KEY                                                      
         J     DIRCTRY                                                          
*                                                                               
VSEQ     LA    RF,DMRSEQ                                                        
         J     DIRCTRY                                                          
*                                                                               
VHIGH    LA    RF,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         J     DIRCTRY                                                          
*                                                                               
VADD     LA    RF,DMADD                                                         
         J     DIRCTRY                                                          
*                                                                               
VWRITE   LA    RF,DMWRT                                                         
*                                                                               
DIRCTRY  NTR1                                                                   
         ST    RF,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,,PRTDIR,KEY,KEY                                     
         J     DMCHECK                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* COMMUNICATION WITH DATA MANAGER (FILE)                                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VGETPRT  LA    RF,GETREC                                                        
         J     FILE                                                             
*                                                                               
VPUTPRT  LA    RF,PUTREC                                                        
         J     FILE                                                             
*                                                                               
VADDPRT  LA    RF,ADDREC                                                        
         J     FILE                                                             
*                                                                               
FILE     NTR                                                                    
         LA    R2,KEY+27                                                        
         CLI   0(RF),C'A'                                                       
         JNE   *+8                                                              
         LA    R2,KEY                                                           
         ST    RF,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,,PRTFILE,(R2),AREC,DMWORK                           
         J     DMCHECK                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* COMMUNICATION WITH DATA MANAGER (PUBDIR)                                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VREADPB  LA    RF,DMREAD                                                        
         MVC   KEYSAVE,KEY                                                      
         J     PUBDIRY                                                          
*                                                                               
VSEQPB   LA    RF,DMRSEQ                                                        
         J     PUBDIRY                                                          
*                                                                               
VHIGHPB  LA    RF,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         J     PUBDIRY                                                          
*                                                                               
VWRIPB   LA    RF,DMWRT                                                         
         J     PUBDIRY                                                          
*                                                                               
VADDPB   LA    RF,DMADD                                                         
         J     PUBDIRY                                                          
*                                                                               
PUBDIRY  NTR                                                                    
*                                                                               
         ST    RF,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,,PUBDIR,KEY,KEY                                     
         J     DMCHECK                                                          
*                                                                               
* COMMUNICATION WITH DATA MANAGER (PUBFILE)                                     
*                                                                               
VGETPUB  LA    RF,GETREC                                                        
         J     PUBFIL                                                           
*                                                                               
VPUTPUB  LA    RF,PUTREC                                                        
         J     PUBFIL                                                           
*                                                                               
VADDPUB  LA    RF,ADDREC                                                        
         J     PUBFIL                                                           
*                                                                               
PUBFIL   NTR                                                                    
         LA    R2,KEY+27                                                        
         CLI   0(RF),C'A'                                                       
         JNE   *+8                                                              
         LA    R2,KEY                                                           
         ST    RF,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,,PUBFILE,(R2),AREC,DMWORK                           
         J     DMCHECK                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* DATA MANAGER ERRORS AND EXIT                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         JNZ   DMERRS                                                           
         XIT                                                                    
*                                                                               
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3               LET GETMSG SORT IT OUT                       
         J     ERROR                                                            
*                                                                               
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
*                                                                               
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
*                                                                               
         OI    6(R2),OI1C          INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         OI    6(R4),X'80'                                                      
         J     EXXMODX                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EXXMOD   TM    ADBSW,AS_ADBRQ      ADBUYER REQUEST?                             
         JZ    EXXMODX             NO                                           
*                                                                               
         OC    SVERRFLD,SVERRFLD   ERROR IN DETECTED?                           
         BNZ   *+12                                                             
         LHI   R0,D#REQID                                                       
         STH   R0,SVERRFLD         DEFAULT TO REPORT ID                         
*                                                                               
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#SR2ERR)              
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ERRNUM),    +        
               ('LD_UBINQ',SVERRFLD),(L'SVERRFLD,0)                             
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ERRDSC),    +        
               ('LD_CHARQ',PRQMSG),(L'PRQMSG,0)                                 
*                                                                               
ALLDONE  GOTOR ALINKIO,DMCB,('LIOACLO',LIOBD)                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
EXXMODX  MVI   DATALKSW,0          RESET DATA LOCKED SWITCH                     
         BRAS  RE,UNLOCK           UNLOCK KEY SET IN THIS APPLICATION           
*                                                                               
         XMOD1 1                                                                
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
EXIT     XIT1                                                                   
*                                                                               
NXTEL    SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         JE    NXTELX              CC IS EQUAL                                  
         CLI   0(R2),0                                                          
         JNE   NXTEL                                                            
         LTR   R2,R2               CC IS NOT EQUAL                              
NXTELX   BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB,R3                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITWKST NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,VCOMFACS                                                      
         MVC   ALINKIO,CLINKIO-COMFACSD(RF)                                     
*                                                                               
         LR    RF,RC                                                            
         A     RF,=A(WRKRECA-PPSR200D)                                          
         ST    RF,AWRKREC                                                       
*                                                                               
         LR    RF,RC                                                            
         A     RF,=A(ESRDS-PPSR200D)                                            
         ST    RF,AESRDSST         ADDRESS OF ESR STORAGE BLOCK                 
*                                                                               
         XC    NUMPRCIN,NUMPRCIN   NUMBER OF PROCESSED INSERTIONS               
         MVI   ADRPYREC,0          ESR REPLY SWITCHS                            
         MVI   ADRPYSW1,0                                                       
         MVI   ADRPYSW2,0                                                       
         MVI   ADRPYSW3,0                                                       
         XC    SVESR#,SVESR#       ESR #                                        
         XC    SVESR_R#,SVESR_R#   ESR REVISION #                               
         XC    ESRDISKA,ESRDISKA   ESR MASTER RECORD DISK ADDRESS               
*                                                                               
         LA    RE,BUYDALST         CLEAR BUY DISK ADDRESS TABLE                 
         LHI   RF,BUYDAMXQ*4                                                    
         XCEFL                                                                  
*                                                                               
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
*                                                                               
         L     RF,=V(PPBYOUT)                                                   
         A     RF,RELO00                                                        
         ST    RF,APPBYOUT                                                      
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,(CXSORT-COMFACSD)(RF)                                         
         ST    RF,XSORT                                                         
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,(CGLOBBER-COMFACSD)(RF)                                       
         ST    RF,VGLOBBER                                                      
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,(CCUREDIT-COMFACSD)(RF)                                       
         ST    RF,VCUREDIT                                                      
*                                                                               
         MVC   FULL,=X'D9000AAB'                                                
         BRAS  RE,GETCORES                                                      
         MVC   GETINS,DMCB         STORE GETINS ADDRESS                         
*                                                                               
         MVC   FULL,=X'D9000AB8'                                                
         BRAS  RE,GETCORES                                                      
         MVC   APUBVAL,DMCB        STORE PUBVAL ADDRESS                         
*                                                                               
         MVC   FULL,=X'D9000AB9'                                                
         BRAS  RE,GETCORES                                                      
         MVC   APUBEDIT,DMCB       STORE PUBEDIT ADDRESS                        
*                                                                               
         MVC   FULL,=X'D9000ABA'                                                
         BRAS  RE,GETCORES                                                      
         MVC   APPGETCG,DMCB       STORE PPGETCG ADDRESS                        
*                                                                               
         MVC   FULL,=X'D9000ABB'                                                
         BRAS  RE,GETCORES                                                      
         MVC   APPGETAD,DMCB       STORE PPGETADR ADDRESS                       
*                                                                               
         GOTOR VCALLOV,DMCB,(X'05',0),(RA)                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VT42105,0(R1)                                                    
*                                                                               
* SET UP LIST OF COMMON ROUTINES ALL ARE LOCATED IN THIS MODULE                 
*                                                                               
         LA    R2,VCOMMON          COMMON ENTRY POINT                           
         SR    R3,R3               INIT ROUTINE ID                              
         LA    R4,SYSCOMM          START OF ADDRESS AVEAREA                     
         LA    R5,VCOUNT           SET NUMBER OF ROUTINES                       
*                                                                               
INIWK60  ST    R2,0(R4)            SET ENTRY POINT                              
         STC   R3,0(R4)            SET ROUTINE ID                               
         LA    R3,4(R3)            BUMP ROUTINE ID                              
         LA    R4,4(R4)            BUMP TO NEXT ADDRESS SAVEAREA                
         BCT   R5,INIWK60                                                       
*                                                                               
         XC    SVSR2PID,SVSR2PID   PASSWORD ID NUMBER CLEARED                   
         L     RF,VCOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTOR CGETFACT,DMCB,(2,0),0,0                                          
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   SVSECAGY,FATAGYSC   ALSO NEEDED TO GET CORRECT PID               
         TM    FATFLAG,X'08'       CHECK IF SECET CODE IS THERE                 
         BZ    *+10                                                             
         MVC   SVSR2PID,FAPASSWD   SAVE PASSWORD ID NUMBER                      
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,RF,R1                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETCORES NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),FULL      CORE-RESIDENT PHASE TO BE CALLED             
         GOTO1 VCALLOV,DMCB                                                     
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         J     EXIT                 RETURN ADDRESS IN DMCB                      
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* CHECK FOR CALL FROM LINK TO INSERTION ORDER                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKADBYER NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    VGLOBBER,VGLOBBER   MUST HAVE GLOBBER ADDRESS                    
         BZ    CKCALLER            RETURN NO CALLS                              
*                                                                               
         XC    GLOBWORK,GLOBWORK                                                
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',GLOBWORK,GLVXLENQ,GLVXCTL                 
         CLI   DMCB+8,GLEGNF                                                    
         BE    CKCALLER            RETURN NO CALLS                              
         CLI   DMCB+8,0            CAN'T HANDLE OTHER ERRORS                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   GLOBWORK(12),=C'PRILINPRISR2'                                    
         BNE   CKCALLER                                                         
*                                                                               
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         XC    LIOBD(LIOBVIS-LIOBD),LIOBD                                       
         LA    R0,LIOBD+L'LIOB                                                  
         ST    R0,LIOBAREC                                                      
         AHI   R0,8000             MAX SIZE FOR LINKIO REC USES                 
         ST    R0,LIOBABUF                                                      
         MVC   LIOBACOM,VCOMFACS                                                
         LA    RF,MAP                                                           
         STCM  RF,15,LIOBAMAP                                                   
         MVI   LIOBMSYS,4          PRINT SYSTEM MSGS                            
         LA    R0,T421FFD                                                       
         ST    R0,LIOBASB2                                                      
         OI    LIOBINDS,LIOBIMLT+LIOBINRM                                       
         XC    LIOBRECL,LIOBRECL   DEFAULT TO SIZE OF MAX BUFFER                
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAINI',LIOBD)                                   
         BNE   CKCALLER                                                         
         OI    ADBSW,AS_ADBRQ      INDICATE CALLED BY ADBUYER                   
         OI    ADBSW,AS_ESRUQ      ESR UPLOAD                                   
*                                                                               
CKCALLX  J     SETCCEQ             EQUAL                                        
*                                                                               
CKCALLER J     SETCCNEQ            NOT EQUAL (NO CALLS FROM DDLINK)             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
MAP      DC    0XL(LIORL)                                                       
         DC    AL2(M#UL_SR2,E#SR2HDR,ESRSRVN-MAP)                               
MAPX     DC    AL2(0)                                                           
*                                                                               
       ++INCLUDE PPMAPSR2          ENHANCED SPACE RESERVATION MAP CODES         
*                                                                               
         LTORG                                                                  
         DROP  RB,R3                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* INITIALIZE TABLE OF SERIAL#S FROM INSERTION ORDER REQUEST                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
S#TB_INI NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   FULL,ASER#TAB       SAVE DATA INDEX TO SERIAL#S                  
*                                                                               
         LR    R0,RC                                                            
         A     R0,=A(SER#TAB-PPSR200D)                                          
         ST    R0,ASER#TAB                                                      
         LHI   R1,SER#TABL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RE,FULL             POINT TO DATA INDEX                          
         SR    RF,RF                                                            
         ICM   RF,3,6(RE)          NUMBER OF SERIAL#S                           
         CHI   RF,MAXSER#Q                                                      
         BNH   *+6                                                              
         DC    H'0'                                                             
         LA    RE,8(RE)            POINT TO 1ST SERIAL#                         
*                                                                               
         L     R1,ASER#TAB                                                      
         USING SER#TABD,R1                                                      
         STCM  RF,3,NUMSER#S       SAVE NUMBER OF SERIAL#S IN TABLE             
*                                                                               
S#TB30   MVI   S#STATUS,S#NOTU_Q   INIT TO NOT USED                             
         MVI   S#MODCOD,0          NO MODIDICATION CODE YET                     
         XC    S#PRCCNT,S#PRCCNT   INIT ORDER OF PROCESSED COUNTER              
         MVC   S#SERIAL,0(RE)      PLACE SERIAL# IN TABLE                       
         LA    RE,5(RE)            POINT TO NEXT INPUT SERIAL#                  
         LA    R1,SER#TBLQ(R1)     POINT TO NEXT BLANK ENTRY IN TABLE           
         BCT   RF,S#TB30           LOOP FOR MORE                                
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R1                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* COMMON ENTRY POINT FOR GENERAL SYSTEM ROUTINES                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VCOMMON  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SRL   RF,24               SHIFT ROUTINE ID TO RIGHT NYBBLE             
         L     RF,VBRANCH(RF)      GET A(ROUTINE)                               
         A     RF,RELO00           RELOCATE ADDRESS                             
*                                                                               
         BASR  RE,RF               GO TO ROUTINE                                
*                                                                               
VCOMMONX J     EXIT                                                             
*                                                                               
* COMMON ROUTINE ADDRESSES                                                      
*                                                                               
VBRANCH  DS    0D                  ALIGNMENT                                    
         DC    A(VREAD)                                                         
         DC    A(VSEQ)                                                          
         DC    A(VHIGH)                                                         
         DC    A(VADD)                                                          
         DC    A(VWRITE)                                                        
         DC    A(VGETPRT)                                                       
         DC    A(VPUTPRT)                                                       
         DC    A(VADDPRT)                                                       
         DC    A(VREADPB)                                                       
         DC    A(VSEQPB)                                                        
         DC    A(VHIGHPB)                                                       
         DC    A(VADDPB)                                                        
         DC    A(VWRIPB)                                                        
         DC    A(VGETPUB)                                                       
         DC    A(VPUTPUB)                                                       
         DC    A(VADDPUB)                                                       
*                                                                               
VCOUNT   EQU   (*-VBRANCH)/4                                                    
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RPYTRAIL NTR1  BASE=*,LABEL=*      REPLY ESR TRAILER DATA                       
*                                                                               
         TM    ADBSW,AS_ESRUQ      ESR UPLOAD?                                  
         JZ    EXIT                                                             
         OC    NUMPRCIN,NUMPRCIN   ANY INSERTION GOT PROCESSED?                 
         JZ    EXIT                                                             
*                                                                               
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
*                                                                               
         TM    ADRPYREC,RPYTRALQ                                                
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#SR2TRA)              
         OI    ADRPYREC,RPYTRALQ                                                
*                                                                               
         L     R4,AESRDSST         POINT TO ESR STORAGE BLOCK                   
         USING ESRDSD,R4                                                        
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ESRLKY),    +        
               ('LD_CHARQ',H_SRLKEY),('H_SRLKYL',0)                             
*                                                                               
         MVC   WORK(L'SVSTDCMC),SVSTDCMC                                        
         LHI   RE,E#REQDCM                                                      
         STH   RE,HALF                                                          
         BRAS  RE,GTSTDCMR                                                      
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R3,R4                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GTSTDCMR NTR1  BASE=*,LABEL=*      GET STANDARD COMMENT RECORD                  
*                                                                               
         OC    WORK(L'PCOMKNUM),SPACES                                          
         CLC   WORK(L'PCOMKNUM),SPACES                                          
         JE    EXIT                                                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING PCOMKEY,RE                                                       
         MVC   PCOMKAGY,QAGENCY                                                 
         MVC   PCOMKMED,QMEDIA                                                  
         MVI   PCOMKRCD,X'40'      STANDARD COMMENT RECORD CODE                 
         MVC   PCOMKNUM,WORK                                                    
         GOTOR HIGH                                                             
         CLC   KEY(L'PCOMKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                INVALID COMMENT CODE                         
*                                                                               
         LH    RF,HALF             SUB-RECORD CODE                              
         CHI   RF,E#COMVAR                                                      
         JE    GTSCM20                                                          
         MVI   BYTE3,QCOMSTDQ      STANDARD COMMENT                             
         CHI   RF,E#STDCOM                                                      
         JE    GTSCM20                                                          
         MVI   BYTE3,QCOMREQQ      REQUEST COMMENT                              
         CHI   RF,E#REQDCM                                                      
         JE    GTSCM20                                                          
*                                                                               
         J     GTSCM40                                                          
*                                                                               
GTSCM20  L     R3,AWRKREC          USE RUN COMMAND TO REPLY E#COMVAR            
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTSRU',M#DLCOM)               
         XC    DUB,DUB                                                          
         LA    RE,KEY                                                           
         USING PCOMKEY,RE                                                       
         MVC   DUB+0(1),PCOMKMED   MEDIA CODE                                   
         MVC   DUB+1(6),PCOMKNUM   COMMENT CODE                                 
         DROP  RE                                                               
*                                                                               
         GOTOR ALINKIO,(R1),('LIOAPUT',LIOBD),('LIOTRAW',001),         +        
               ('LD_CHARQ',DUB),(L'PCOMKNUM+L'PCOMKMED,0)                       
*                                                                               
         CLI   BYTE3,0             DEFAULT COMMENT?                             
         JE    GTSCM28                                                          
         GOTOR ALINKIO,(R1),('LIOAPUT',LIOBD),('LIOTRAW',002),         +        
               ('LD_UBINQ',BYTE3),(L'BYTE3,0)                                   
*                                                                               
GTSCM28  GOTOR ALINKIO,(R1),('LIOAPUT',LIOBD),('LIOTERU',0),0,0                 
         J     EXIT                                                             
*                                                                               
GTSCM40  MVC   FULL,AREC           SAVE ORIGINAL AIO POINTER                    
         LA    RE,PREPREC          SAFE TO USE THIS REC AREA                    
         ST    RE,AREC                                                          
         GOTOR GETPRT                                                           
         MVC   AREC,FULL           RESTORE ORIGINAL AIO POINTER                 
         LA    R2,PREPREC+33                                                    
         CLI   0(R2),X'40'                                                      
         BE    *+6                                                              
         DC    H'0'                INVALID STANDARD COMMENT RECORD              
         MVI   ELCODE,X'40'        COMMENT ELEM CODE                            
         BRAS  RE,RPYCOMLN         REPLY COMMENT LINES                          
         BRAS  RE,NXTEL                                                         
         BE    *-8                                                              
*                                                                               
         J     EXIT                                                             
*                                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB,R3                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GTCONCMR NTR1  BASE=*,LABEL=*      GET CONTRACT COMMENT RECORD                  
*                                                                               
         OC    WORK(L'PCLTKCLT+L'PCLTOFF),SPACES                                
         CLC   WORK(L'PCLTKCLT+L'PCLTOFF),SPACES                                
         JE    EXIT                                                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING PCNCKEY,R2                                                       
         MVC   PCNCKAGY,QAGENCY                                                 
         MVC   PCNCKMED,QMEDIA                                                  
         MVI   PCNCKRCD,X'44'      CONTRACT COMMENT RECORD CODE                 
         MVC   SVWRKKEY,PCNCKEY                                                 
         MVC   PCNCKCLI,WORK       TRY CLIENT SPECIFIC                          
         GOTOR HIGH                                                             
         CLC   KEY(L'PCNCKEY),KEYSAVE                                           
         BE    GTCCM30                                                          
*                                                                               
         MVC   PCNCKEY,SVWRKKEY    TRYING FOR OFFICE SPECIFIC                   
         MVI   PCNCKCLI,X'FF'                                                   
         MVC   PCNCKCLI+1(L'PCLTOFF),WORK+L'PCLTKCLT                            
         OC    PCNCKCLI,SPACES                                                  
         GOTOR HIGH                                                             
         CLC   KEY(L'PCNCKEY),KEYSAVE                                           
         BE    GTCCM30                                                          
*                                                                               
         MVC   PCNCKEY,SVWRKKEY    TRYING FOR ALL CLIENTS                       
         MVC   PCNCKCLI,=X'FFFFFF'                                              
         GOTOR HIGH                                                             
         CLC   KEY(L'PCNCKEY),KEYSAVE                                           
         JNE   EXIT                                                             
         TM    ADRPYSW4,ALLCCCMQ   ALL CLIENT CONCOM REPLIED ALREADY?           
         JNZ   EXIT                                                             
         OI    ADRPYSW4,ALLCCCMQ   SET TO PROCESS ONLY ONCE                     
*                                                                               
GTCCM30  MVC   FULL,AREC           SAVE ORIGINAL AIO POINTER                    
         LA    RE,PBUYREC          SAFE TO USE THIS REC AREA                    
         ST    RE,AREC                                                          
         GOTOR GETPRT                                                           
         MVC   AREC,FULL           RESTORE ORIGINAL AIO POINTER                 
         LA    R2,PBUYREC+33                                                    
         CLI   0(R2),X'40'                                                      
         BE    *+6                                                              
         DC    H'0'                INVALID STANDARD COMMENT RECORD              
         MVI   ELCODE,X'40'        COMMENT ELEM CODE                            
*                                                                               
GTCCM36  CLC   2(4,R2),=C'COM='    EMBEDDED COMMENTS?                           
         BNE   GTCCM40                                                          
         LA    RE,2+4(R2)          POINT TO START OF COMMENT CODE               
         LR    RF,RE                                                            
GTCCM36K CLI   0(RF),C','          END OF CURRENT COMMENT CODE?                 
         BE    GTCCM36M                                                         
         CLI   0(RF),0             END OF RECORD?                               
         BE    GTCCM36M                                                         
         CLI   0(RF),X'40'         NEXT ELEM?                                   
         BE    GTCCM36M                                                         
         LA    RF,1(RF)            BUMP TO NEXT CHAR                            
         B     GTCCM36K                                                         
GTCCM36M STCM  RF,15,DOUBLE        SAVE ELEM POINTER                            
         SR    RF,RE               LENGTH OF COMMENT CODE                       
         BNP   GTCCM36P            BAD LENGTH, DO NEXT COMMENT CODE             
         CHI   RF,6                                                             
         BH    GTCCM36P            BAD LENGTH, DO NEXT COMMENT CODE             
         MVC   WORK,SPACES                                                      
         LA    R1,WORK+L'PCOMKNUM                                               
         SR    R1,RF                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)       COMMENT CODE - RIGHT ALIGN                   
         BRAS  RE,GTSTDCMR         GET STANDARD COMMENTS                        
*                                                                               
GTCCM36P ICM   RF,15,DOUBLE                                                     
         CLI   0(RF),0             END OF RECORD?                               
         BE    GTCCM_X                                                          
         CLI   0(RF),X'40'         NEXT ELEM?                                   
         BE    GTCCM46                                                          
         CLI   0(RF),C','          END OF PREVIOUS COMMENT CODE?                
         BE    *+6                                                              
         DC    H'0'                BAD POINTER                                  
         LA    RF,1(RF)                                                         
         CLC   0(4,RF),=C'COM='    ANOTHER EMBEDDED COMMENT CODE?               
         BNE   GTCCM46             BAD ELEM, BUT SKIP IT                        
         LA    RF,4(RF)            PASS OVER HEAD                               
         LR    RE,RF                                                            
         B     GTCCM36K            GO BACK AND DO MORE                          
*                                                                               
GTCCM40  BRAS  RE,RPYCOMLN         REPLY COMMENT LINES                          
GTCCM46  BRAS  RE,NXTEL                                                         
         BE    GTCCM36                                                          
*                                                                               
GTCCM_X  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R2                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RPYCOMLN NTR1  BASE=*,LABEL=*      REPLY COMMENT LINES                          
*                                                                               
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         LHI   R4,1                FOR LOOPING                                  
*                                                                               
         CLC   2(05,R2),=C'SHIP='                                               
         BE    R_COML_X                                                         
         CLC   2(06,R2),=C'LABEL='                                              
         BE    R_COML_X                                                         
*                                                                               
         LH    RF,HALF             SUB-RECORD CODE                              
         CHI   RF,E#REQDCM                                                      
         BE    R_COML34                                                         
         CHI   RF,E#STDCOM                                                      
         BE    R_COML36                                                         
         CHI   RF,E#COMVAR                                                      
         BE    R_COML38                                                         
*                                                                               
         DC    H'0'                NO OTHER SUB-RECORD CODE                     
*                                                                               
R_COML34 TM    ADRPYSW3,RPYRSCMQ   REQ STD COMM SUB-REC CODE REPLIED?           
         BNZ   R_COML60                                                         
         OI    ADRPYSW3,RPYRSCMQ                                                
         B     R_COML50                                                         
*                                                                               
R_COML36 TM    ADRPYSW2,RPYSTDCQ   REQ STD COMM SUB-REC CODE REPLIED?           
         BNZ   R_COML60                                                         
         OI    ADRPYSW2,RPYSTDCQ                                                
         B     R_COML50                                                         
*                                                                               
R_COML38 TM    ADRPYSW2,RPYVARCQ   VARIOUS COMM SUB-REC CODE REPLIED?           
         BNZ   R_COML60                                                         
         OI    ADRPYSW2,RPYVARCQ                                                
         B     R_COML50                                                         
*                                                                               
         DC    H'0'                NO OTHER SUB-RECORD CODE                     
*                                                                               
R_COML50 GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',(RF))                  
*                                                                               
R_COML60 CLI   1(R2),3             EMPTY COMMENT LINE?                          
         BH    *+16                                                             
R_COML62 LA    R2,SPACES                                                        
         LHI   R5,1                                                             
         B     R_COML80                                                         
         CLI   2(R2),C'+'          SKIPPING LINES?                              
         BNE   R_COML66                                                         
         PACK  DUB,3(1,R2)                                                      
         TP    DUB                                                              
         BNZ   *+8                                                              
         CVB   R4,DUB                                                           
         LTR   R4,R4                                                            
         BNZ   R_COML62                                                         
R_COML66 SR    R5,R5                                                            
         IC    R5,1(R2)                                                         
         SHI   R5,2                                                             
         LA    R2,2(R2)                                                         
         CLI   REMCHRCM,C'Y'       12A - REMOVE SPECIAL CHARS?                  
         BNE   R_COML80                                                         
         CLI   0(R2),C'*'                                                       
         BE    *+8                                                              
         CLI   0(R2),C'+'                                                       
         BE    *+8                                                              
         CLI   0(R2),C'#'                                                       
         BE    *+8                                                              
         CLI   0(R2),C'@'                                                       
         BE    *+8                                                              
         CLI   0(R2),C'&&'                                                      
         BE    *+8                                                              
         B     R_COML80                                                         
         LA    R2,1(R2)            PASS SPECIAL CHAR                            
         BCTR  R5,0                ONE LESS CHAR                                
*                                                                               
R_COML80 L     RF,LIOBAREC                                                      
         CLC   0(2,RF),=H'7000'                                                 
         JNH   *+6                                                              
         DC    H'0'                                                             
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#COMMNT),    +        
               ('LD_CHARQ',0(R2)),((R5),0)                                      
         BCT   R4,R_COML80                                                      
*                                                                               
R_COML_X J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R3                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
UNLOCK   NTR1  BASE=*,LABEL=*      UNLOCKS LOCKS PUT OUT EARLIER                
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
*                                                                               
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BC'    CLIENT LOCK                                  
         MVC   L.LOCKMED,PRQMED                                                 
         MVC   L.LOCKCLT,PRQCLT                                                 
*                                                                               
         CLC   PRQPUB(3),=C'ALL'   PUB IS ALL?                                  
         BE    UNLK2               YES, GO AHEAD AND UNLOCK CLT LOCK            
         OC    BPUB(4),BPUB        NOTHING IN BASE PUB NUMBER?                  
         BZ    UNLK2               YES, GO AHEAD AND UNLOCK CLT LOCK            
         MVC   L.LOCKRTY,=C'BP'    PUB LOCK                                     
         MVC   L.LOCKPUB,BPUB                                                   
         XC    L.LOCKPUB,=4X'FF'   COMPLEMENT PUB (NO BINARY ZERO)              
*                                                                               
UNLK2    L     RF,VCOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKUNLKQ',LKUPKEY),VCOMFACS                           
*                                                                               
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    UNLK2                                                            
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB,L                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PPSR200D DSECT                                                                  
*                                                                               
POLWRKQ  EQU   POLWRKX-PPWORK                                                   
         DS    (POLWRKQ)X                                                       
*                                                                               
ESRWORKQ EQU   ESRWORKX-ESRWORK                                                 
         DS    (ESRWORKQ)X                                                      
*                                                                               
POLFILEQ EQU   POLFILEX-POLFILE                                                 
         DS    (POLFILEQ)X                                                      
*                                                                               
PPSR2WKQ EQU   PPSR2REX-PPSR2REC                                                
         DS    (PPSR2WKQ)X                                                      
*                                                                               
DUMDUM1Q EQU   DUMDUM1X-DUMDUM1                                                 
         DS    (DUMDUM1Q)X                                                      
*                                                                               
WRKRECA  DS    (WRKRECAL)X                                                      
WRKRECAL EQU   18432+8000                                                       
WRKRECAX EQU   *                                                                
*                                                                               
SER#TAB  DS    (SER#TABL)X                                                      
SER#TABL EQU   MAXSER#Q*SER#TBLQ                                                
SER#TABX EQU   *                                                                
*                                                                               
ESRDS    DS    (ESRDSDLQ)X         ESR DATA STREAM STORAGE AREA                 
*                                                                               
PPSR200X EQU   *                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPSR2WRK1                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPSR2WRK2                                                      
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011PPSR200   01/28/14'                                      
         END                                                                    
