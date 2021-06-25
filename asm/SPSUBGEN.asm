*          DATA SET SPSUBGEN   AT LEVEL 011 AS OF 10/23/14                      
*PHASE SPSBGENA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE PRTREC                                                                 
*INCLUDE SORTER                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE LOADER                                                                 
*INCLUDE CALLOFF                                                                
*INCLUDE GETPROF                                                                
                                                                                
*=============================================================                  
* REQUIRED INPUT CARDS:                                                         
* AGENCY=XY                                                                     
* SPOTFILE=SPOTX                                                                
* OPTIONAL INPUT CARDS:                                                         
* EXTRACT=I/J/F                                                                 
* EXCLUDEJ1=Y/N TO MAKE PROGRAM LOOK AT J1 EXCLUSION OPTION                     
* DB= TO OVERRIDE DB PROFILE                                                    
* DF= TO OVERRIDE DF PROFILE                                                    
*=============================================================                  
         TITLE 'SPSUBGEN - GENERATE DB2 T/A REQUEST CARDS'                      
SPSUBGEN CSECT                                                                  
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
         ENTRY COMFACS                                                          
*                                                                               
         NBASE SBGNWKLQ,SPSUBGEN,=V(REGSAVE),RA                                 
         USING SBGENWKD,RC                                                      
*                                                                               
         LAY   RF,CLTREC                                                        
         ST    RF,ADCLT                                                         
*                                                                               
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
         LA    RE,SPSUBGEN          SET FOR STXITER                             
         L     RF,=V(STXITER)                                                   
         STM   RE,RF,DUB                                                        
         OI    DUB+4,X'80'                                                      
         GOTO1 =V(STXITER),DMCB,DUB                                             
*                                                                               
         MVC   DUB(8),=CL8'T00A15' LOAD CLUNPK                                  
         GOTO1 =V(LOADER),DMCB,DUB,0                                            
         MVC   VCLUNPK,4(R1)                                                    
*                                                                               
         MVC   DUB(8),=CL8'T00A38' LOAD OFFICER                                 
         GOTO1 =V(LOADER),DMCB,DUB,0                                            
         MVC   VOFFICER,4(R1)                                                   
*                                                                               
         MVC   DUB(8),=CL8'T00A7A' LOAD STAPACK                                 
         GOTO1 =V(LOADER),DMCB,DUB,0                                            
         MVC   VSTAPACK,4(R1)                                                   
*                                                                               
         MVC   DUB(8),=CL8'T00A9E' LOAD CABLETAB                                
         GOTO1 =V(LOADER),DMCB,DUB,0                                            
         MVC   VT00A9E,4(R1)                                                    
*                                                                               
         LAY   R1,STAWORK          PASS A(T00A9E) TO STAPACK                    
         XC    0(L'STAWORK,R1),0(R1)                                            
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,QSTP_T00A9E                                              
         MVC   STAPACOM,VT00A9E                                                 
         GOTO1 VSTAPACK,(R1)                                                    
         DROP  R1                                                               
*                                                                               
         EJECT                                                                  
*==================================================================*            
*  READ PARAMETER CARDS O                                                       
*==================================================================*            
*                                                                               
         MVI   TRACEFLG,X'00'                                                   
*                                                                               
NXTCARD  GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD                                                      
         BE    ENDCARD                                                          
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLC   =C'DDSIO=',CARD                                                  
         BNE   NXTCARD1                                                         
         L     RE,=V(DDSIO)                                                     
         MVC   0(8,RE),CARD+6                                                   
         B     NXTCARD                                                          
*                                                                               
         USING SSBOFFD,RE                                                       
NXTCARD1 CLC   =C'DSPACE=',CARD                                                 
         BNE   NXTCARD2                                                         
         L     RE,=V(SSB)                                                       
         MVC   SSODSPAC,CARD+7                                                  
         B     NXTCARD                                                          
         DROP  RE                                                               
                                                                                
NXTCARD2 CLC   =C'EXTRACT=',CARD                                                
         BNE   NXTCARD3                                                         
         MVC   EXTRACTC,CARD+8                                                  
         CLI   CARD+8,C'F'                                                      
         BE    NXTCARD                                                          
         CLI   CARD+8,C'I'                                                      
         BE    NXTCARD                                                          
         CLI   CARD+8,C'J'                                                      
         BE    NXTCARD                                                          
         B     GETSERR                                                          
*                                                                               
NXTCARD3 CLC   =C'EXCLUDEJ1=',CARD                                              
         BNE   NXTCARD4                                                         
         MVC   EXCLUDJ1,CARD+10                                                 
         CLI   CARD+10,C'N'                                                     
         BE    NXTCARD                                                          
         CLI   CARD+10,C'Y'                                                     
         BE    NXTCARD                                                          
         B     GETSERR                                                          
*                                                                               
NXTCARD4 CLC   =C'TRACE=',CARD                                                  
         BNE   GETSYS                                                           
         CLI   CARD+6,C'Y'                                                      
         BNE   NXTCARD                                                          
         OI    TRACEFLG,X'01'                                                   
         B     NXTCARD                                                          
*                                                                               
GETSYS   DS    0H                  GET SYSTEM NUMBER                            
         CLC   =C'SPOT',CARD                                                    
         BE    GETSE                                                            
*                                                                               
         CLC   =C'DB=',CARD        DB PROFILE OVERRIDE                          
         BE    *+14                                                             
         CLC   =C'DF=',CARD        DF PROFILE OVERRIDE                          
         BNE   GETSERR                                                          
         MVC   SVDBPROF,CARD+3                                                  
         B     NXTCARD                                                          
*                                                                               
GETSERR  MVC   P(20),=CL20'INVALID INPUT CARD: '                                
         MVC   P+20(60),CARD                                                    
         GOTO1 =V(PRINTER)                                                      
         B     ENDPROG                                                          
         EJECT                                                                  
***********************************************************************         
* LOCATE SPOT SYSTEM AND GET SENUM. EXTRACT AGENCY ALPHA ALSO.        *         
* CONVERT TO USE TWO CHR SYSTEM NAME SPOTXYAA WHERE XY=SYS AND AA=AGY *         
***********************************************************************         
         SPACE 1                                                                
GETSE    CLI   CARD+7,C' '         SPOTXAA > SPOTX AA                           
         BNE   GETSE1                                                           
         CLI   CARD+6,C' '                                                      
         BE    GETSE1                                                           
         ICM   R0,3,CARD+5         MOVE AGY CODE ONE BYTE TO RIGHT              
         MVI   CARD+5,C' '                                                      
         STCM  R0,3,CARD+6                                                      
*                                                                               
GETSE1   MVC   SESNAM+7(2),CARD+4  EXTRACT ONE/TWO CHR SYSTEM ID                
*                                                                               
GETSEX   GOTO1 =V(DMDDNAME),DMCB,(X'24',=C'DDNAME'),SESNAM,0                    
         CLI   8(R1),0                                                          
         BNE   GETSERR                                                          
         L     RF,8(R1)            GET A(FILE INFO LIST)                        
         MVC   SESNUM,1(RF)        EXTRACT SENUM                                
         L     RE,=V(UTL)          MOVE TO UTL                                  
         MVC   4(1,RE),SESNUM                                                   
*                                                                               
         MVC   FILSAVE,CARD        SAVE SYSTEM NAME                             
         MVC   AGYSAVE,CARD+6      SAVE AGENCY ALPHA                            
         B     NXTCARD                                                          
*                                                                               
SESNAM   DC    C'SYS=SPT##'                                                     
SESNUM   DC    X'00'                                                            
         EJECT                                                                  
ENDCARD  DS    0H                                                               
         LAY   RF,SPTFLIST                                                      
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'SPOT',(RF)                        
         B     RAGY                                                             
*                                                                               
RAGY     LA    R2,KEY                                                           
         USING AGYHDRD,R2          READ AGENCY HEADER                           
         XC    KEY,KEY                                                          
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGYSAVE                                                  
         DROP  R2                                                               
*                                                                               
         BAS   RE,HIGH                                                          
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,RDATA            READ INTO RECOVERY RECORD                    
         ST    R6,AIO                                                           
         BAS   RE,GET                                                           
*                                                                               
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING AGYEL,R6                                                         
         MVC   CTRYSAVE,AGYPCNDA   CANADA                                       
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'        MEDIA CODE ELEMENT                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                NO MEDIA CODE ELEMENT                        
*                                                                               
         MVC   BAGY,3(R6)                                                       
         NI    BAGY,X'F0'          TURN OFF MEDIA                               
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0   INIT SORTER                 
*                                                                               
         OC    SVDBPROF,SVDBPROF   TEST PROFILE OVERRIDE                        
         BNZ   IN0                                                              
         MVC   WORK(12),=CL12'S0DB'                                             
         MVC   WORK+4(3),AGYSAVE                                                
         GOTO1 =V(GETPROF),DMCB,WORK,SVDBPROF,VDATAMGR                          
*                                                                               
IN0      OPEN  (RECVIN,(INPUT))                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (REQOUT,(OUTPUT))                                                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LAY   R1,STAWORK          FINISH STAPACK INIT NOW                      
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'U'        SET FOR UNPACK CALLS                         
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPAGY,AGYSAVE                                                  
         MVC   STAPCTRY,CTRYSAVE                                                
         DROP  R1                                                               
         EJECT                                                                  
*============================================================                   
*** PROCESS INPUT FILE ***                                                      
*============================================================                   
                                                                                
QSPTFIL  EQU   X'21'                                                            
COPY     EQU   1                                                                
CHG      EQU   2                                                                
ADD      EQU   3                                                                
*                                                                               
IN2      LA    R0,DM$RECVHDR-4                                                  
         GET   RECVIN,(0)                                                       
*                                                                               
         CLI   DM$RFILTY,QSPTFIL   SPTFILE ?                                    
         BNE   IN2                                                              
         CLI   DM$RRECTY,X'02'     RECORD CHANGED?                              
         BE    *+12                                                             
         CLI   DM$RRECTY,X'03'     RECORD ADDED?                                
         BNE   IN2                                                              
*                                                                               
         LA    RE,DM$RECVHDR-4     POINT TO RECORD LENGTH                       
         AH    RE,0(RE)            ADD LENGTH                                   
         XC    0(2,RE),0(RE)       CLEAR EOR                                    
*                                                                               
         LA    R2,DM$RECVHDR+24    R2 -> DATA RECORD.DO NOT USE R2 !!!          
*                                                                               
         CLI   TRACEFLG,X'00'                                                   
         BE    IN4                                                              
         MVC   P(10),=CL10'REC='                                                
         GOTO1 =V(HEXOUT),DMCB,0(R2),P+10,13,=C'N'                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
IN4      DS    0H                                                               
         CLI   0(R2),2             TEST GOALREC                                 
         BNE   IN10                                                             
*                                                                               
         USING GOALRECD,R2                                                      
         MVC   BYTE,GKEYAM                                                      
         NI    BYTE,X'F0'          TURN OFF MEDIA                               
         CLC   BYTE,BAGY                                                        
         BNE   IN2                                                              
*                                                                               
         MVC   BAGYMD,GKEYAM                                                    
         MVC   BCLT,GKEYCLT                                                     
         MVI   BPRD,X'FF'                                                       
         MVC   BMKT,GKEYMKT                                                     
         XC    BSTA,BSTA                                                        
         MVC   BEST,GKEYEST                                                     
         B     PROCREC                                                          
         DROP  R2                                                               
*                                                                               
IN10     DS    0H                                                               
         CLI   0(R2),X'10'         TEST BUYREC                                  
         BL    IN20                NO                                           
         TM    0(R2),X'08'         TEST FOR 'COPY' BIT                          
         BO    IN2                 YES-SKIP THIS RECORD                         
*                                                                               
         USING BUYRECD,R2                                                       
*                                                                               
         TM    BDCIND2,BDCCANAQ    CANADIAN BUY?                                
         BZ    *+14                                                             
         OC    BUYKMKTN,BUYKMKTN                                                
         BZ    IN2                                                              
*                                                                               
         MVC   BYTE,BUYKAM                                                      
         NI    BYTE,X'F0'          TURN OFF MEDIA                               
         CLC   BYTE,BAGY           SAME AGENCY AS REQUESTED ?                   
         BNE   IN2                                                              
*                                                                               
         CLI   EXTRACTC,C'F'       TEST FINANCIAL REQUEST                       
         BNE   IN12                                                             
         CLI   DM$RPRG,X'13'       TEST SPOT PAY PROGRAM                        
         BNE   IN2                 NO - CAN'T BE A CLEARANCE                    
*                                                                               
IN12     MVC   BAGYMD,BUYKAM                                                    
         MVC   BCLT,BUYKCLT                                                     
         MVI   BPRD,X'FF'                                                       
         MVC   BMKTSTA,BUYKMSTA                                                 
         MVC   BEST,BUYKEST                                                     
         B     PROCREC                                                          
         DROP  R2                                                               
*                                                                               
IN20     CLI   EXTRACTC,C'F'       TEST FINANCIAL REQUEST                       
         BNE   IN2                                                              
         CLC   =X'0E01',0(R2)      TEST STABUCK RECORD                          
         BNE   IN2                                                              
         USING STABUCKD,R2                                                      
*                                                                               
         MVC   BYTE,STABKAM                                                     
         NI    BYTE,X'F0'          TURN OFF MEDIA                               
         CLC   BYTE,BAGY                                                        
         BNE   IN2                                                              
*                                                                               
         MVC   BAGYMD,STABKAM                                                   
         MVC   BCLT,STABKCLT                                                    
         MVC   BPRD,STABKPRD                                                    
         MVC   BEST,STABKEST                                                    
         XC    BMKTSTA,BMKTSTA                                                  
         DROP  R2                                                               
*                                                                               
PROCREC  DS    0H                  BUILD REQUEST CARD HERE                      
         LA    R3,SORTREC                                                       
         USING REQD,R3                                                          
         MVC   SRTQDATA,SPACES                                                  
         XC    SRTBDATA,SRTBDATA                                                
*                                                                               
         MVC   SRTBAMCL,BAGYMD     MOVE BINARY SORT DATA                        
         MVC   SRTBPRD,BPRD                                                     
         MVC   REQPRD(1),BPRD                                                   
*                                                                               
         MVC   REQTYP,=C'UB'                                                    
         CLI   EXTRACTC,C'F'       TEST FINANCIAL REQ                           
         BNE   *+10                                                             
         MVC   REQTYP,=C'UF'                                                    
*                                                                               
         MVC   REQAGY,AGYSAVE                                                   
*                                                                               
         SR    RE,RE                                                            
         IC    RE,BAGYMD                                                        
         N     RE,=X'0000000F'     DROP AGY                                     
         LA    RE,MDTAB-1(RE)                                                   
         MVC   REQMED,0(RE)                                                     
         MVC   SVREQMED,0(RE)                                                   
*                                                                               
         CLI   CTRYSAVE,C'C'       CANADA?                                      
         BNE   PROCREC1                                                         
         CLI   EXTRACTC,C' '       GOALS/DEMOS EXTRACT?                         
         BH    PROCREC1                                                         
         CLI   0(R2),X'10'         BUY RECORD?                                  
         BL    PROCREC1                                                         
         MVC   BYTE,0(R2)          COPY A/M                                     
         NI    BYTE,X'0F'          TURN OFF AGENCY                              
         CLI   BYTE,X'01'          TV?                                          
         BE    *+12                YES, SET MEDIA TO 'C'                        
         CLI   BYTE,X'03'          NET?                                         
         BNE   PROCREC1            NO - DON'T SET MEDIA TO 'C'                  
         MVI   REQMED,C'C'         ALWAYS MEDIA C FOR CND BUY CHANGES           
*                                                                               
*        CLI   CTRYSAVE,C'C'                                                    
*        BNE   PROCREC1                                                         
*        CLI   REQMED,C'T'                                                      
*        BE    *+12                                                             
*        CLI   REQMED,C'N'                                                      
*        BNE   PROCREC1                                                         
*                                                                               
*        MVI   REQMED,C'C'                                                      
*                                                                               
PROCREC1 DS    0H                                                               
         MVC   SVBCLT,BCLT                                                      
         GOTO1 VCLUNPK,DMCB,BCLT,REQCLT                                         
*                                                                               
         CLI   BPRD,X'FF'                                                       
         BNE   *+10                                                             
         MVC   REQPRD,=C'POL'                                                   
*                                                                               
         MVC   REQMKT,=C'ALL '                                                  
         SR    R0,R0                                                            
         ICM   R0,3,BMKT           MARKET                                       
         BZ    PROCREC2                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  REQMKT,DUB                                                       
*                                                                               
PROCREC2 MVC   REQSTA(3),=C'ALL'                                                
*                                                                               
         CLI   EXTRACTC,C'F'       TEST FINANCIAL REQ                           
         BNE   PROCREC4                                                         
         CLI   0(R2),X'10'         TEST BUY RECORD                              
         BL    PROCREC4                                                         
*                                                                               
         LAY   R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
*                                                                               
*        MVC   STAPMED,REQMED                                                   
         MVC   STAPMED,SVREQMED                                                 
         MVC   STAPSTA,BSTA                                                     
*                                                                               
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   REQSTA,STAPQSTA                                                  
         CLI   REQSTA+4,C' '                                                    
         BH    *+8                                                              
         MVI   REQSTA+4,C'T'                                                    
         CLI   REQSTA,C'0'         TEST CABLE STATION                           
         BL    *+10                NO  -                                        
         MVC   REQSTA,SPACES       THEN REMOVE STATION                          
*                                                                               
         CLI   CTRYSAVE,C'C'                                                    
         BNE   PROCREC4                                                         
         CLI   REQSTA+4,C'/'                                                    
         BNE   PROCREC4                                                         
         MVI   REQSTA+4,C' '                                                    
         MVC   REQSRTNT,STAPQNET                                                
         DROP  R1                                                               
*                                                                               
PROCREC4 SR    R0,R0                                                            
         IC    R0,BEST             ESTIMATE                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  REQEST,DUB                                                       
*                                                                               
         MVI   SRTTYPE,C'B'                                                     
         CLI   0(R2),2                                                          
         BNE   *+8                                                              
         MVI   SRTTYPE,C'G'                                                     
*                                                                               
         CLI   EXTRACTC,C'F'       TEST FINANCIAL REQUEST                       
         BNE   PROCRECX                                                         
*                                                                               
         MVI   SRTTYPE,C'P'        SET PAYABLE REQUEST                          
         CLI   0(R2),X'10'         TEST BUYREC                                  
         BH    PROCRECX            NO                                           
         MVI   SRTTYPE,C'R'        SET RECEIVABLE (BILLING) REQUEST             
*                                                                               
PROCRECX GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
*                                                                               
         CLI   TRACEFLG,X'00'                                                   
         BE    PROCREC9                                                         
         MVC   P(10),=CL10'SRT='                                                
         MVC   P+10(L'SRTQDATA),SORTREC                                         
         GOTO1 =V(HEXOUT),DMCB,SRTBDATA,P+10+1+L'SRTQDATA,6,=C'N'               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PROCREC9 DS    0H                                                               
         CLI   EXTRACTC,C'F'       TEST FINANCIAL REQUEST                       
         BE    IN2                                                              
                                                                                
*============================================================                   
* CHECK FOR SPILL MARKETS                                                       
*============================================================                   
                                                                                
         LA    R2,DM$RECVHDR+24                                                 
         USING BUYRECD,R2                                                       
         LA    R6,BDELEM                                                        
*                                                                               
CHKSPIL2 SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    CHKSPILX                                                         
         CLI   0(R6),3                                                          
         BNE   CHKSPIL2                                                         
         SR    R0,R0                                                            
         ICM   R0,3,4(R6)                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  REQMKT,DUB                                                       
         MVI   SRTSPILL,C'Y'                                                    
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
*                                                                               
         CLI   TRACEFLG,X'00'                                                   
         BE    CHKSPIL4                                                         
         MVC   P(10),=CL10'SRT='                                                
         MVC   P+10(L'SRTQDATA),SORTREC                                         
         GOTO1 =V(HEXOUT),DMCB,SRTBDATA,P+10+1+L'SRTQDATA,6,=C'N'               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
CHKSPIL4 DS    0H                                                               
*                                                                               
         B     CHKSPIL2                                                         
*                                                                               
CHKSPILX B     IN2                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
ENDIN    DS    0H                                                               
         CLOSE (RECVIN,)                                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   SRTQDATA,SPACES     INITIALIZE TO NEQ                            
*                                                                               
GETREQ   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R2,15,DMCB+4                                                     
         BZ    GETREQX             FINISH, IF END OF TABLE                      
*                                                                               
         CLI   TRACEFLG,X'00'                                                   
         BE    GETREQ1                                                          
         MVC   P(10),=CL10'SRT GET='                                            
         MVC   P+10(L'SRTQDATA),0(R2)                                           
         GOTO1 =V(HEXOUT),DMCB,L'SRTQDATA(R2),P+10+1+L'SRTQDATA,6,=C'N'         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
GETREQ1  DS    0H                                                               
*                                                                               
         CLC   SRTQDATA,0(R2)      TEST SAME EBCDIC DATA                        
         BNE   GETREQ2             NO - PROCESS                                 
*                                                                               
         CLI   TRACEFLG,X'00'                                                   
         BE    GETREQ                                                           
         MVC   P(10),=CL10'DUP'                                                 
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         B     GETREQ        <===================                               
*                                                                               
*&&DO                                                                           
         CLI   SRTTYPE,C'R'        TEST RECEIVABLE REQ                          
         BNE   GETREQ              NO - ON TO NEXT                              
*                                                                               
         CLC   SORTREC,0(R2)       THEN MATCH EBCDIC AND BINARY DATA            
         BE    GETREQ                                                           
*&&                                                                             
Q        USING REQD,CARD                                                        
*                                                                               
GETREQ2  MVC   SORTREC,0(R2)       MOVE DATA SO WE CAN SEE IT                   
         MVC   CARD,SPACES                                                      
         MVC   CARD(L'SRTQDATA),SRTQDATA  MOVE EBCDIC DATA                      
*                                                                               
         MVC   Q.REQCBNET,Q.REQSRTNT                                            
         OC    Q.REQCBNET,SPACES                                                
         MVC   Q.REQSRTNT,SPACES                                                
*                                                                               
         MVC   Q.REQESDAT,=C'ES'          COMMON FOR ALL CARDS                  
         MVI   Q.REQOPT1,C'Y'             FIXED LENGTH OUTPUT                   
         MVC   Q.REQOPT2,EXTRACTC                                               
*                                                                               
         CLI   Q.REQOPT2,C'F'             TEST FINANCIAL                        
         BNE   GETREQ10                                                         
         MVC   Q.REQOPT2,SRTTYPE   FINANCIAL SETS TYPE TO P/R                   
*                                                                               
         CLI   SRTTYPE,C'P'        TEST PAYABLE                                 
         BNE   GETREQ10                                                         
*                                                                               
         CLI   Q.REQSTA,C' '       TEST STATION IS BLANK                        
         BNE   GETREQ4             NO - SO IT'S NOT CABLE                       
         MVC   QSAVE(26),SORTREC   SAVE TY/A-M/CL/PR/MK/ST/ES                   
         B     GETREQ10                                                         
*                                                                               
GETREQ4  MVC   WORK(26),SORTREC                                                 
         MVC   WORK+18(5),SPACES   SET STATION TO SPACES                        
         CLC   QSAVE(26),WORK      AND SEE IF HAD ALL MKT REQ                   
         BNE   GETREQ10                                                         
         MVC   P(40),CARD                                                       
         MVC   P+41(26),=C'STATION REQUEST SUPPRESSED'                          
         B     GETREQ22                                                         
*                                                                               
GETREQ10 MVC   Q.REQREQ(11),=CL11'COCHON'  COMMON FOR ALL CARDS                 
*                                                                               
         CLI   SRTSPILL,C'Y'                                                    
         BNE   *+10                                                             
         MVC   Q.REQREQ,=CL12'**SPILL**'                                        
*                                                                               
         BAS   RE,CHKCLT           READ CLTHDR AND TEST TO OUTPUT               
         BNE   GETREQ                                                           
*                                                                               
         MVC   Q.REQCLT,CLIENT     MOVE CORRECT EBCDIC CODE                     
*                                                                               
         CLI   SRTBPRD,X'FF'                                                    
         BE    GETREQ20                                                         
         BAS   RE,GETPRD           GET EBCDIC PRODUCT                           
         MVC   Q.REQPRD,0(R1)                                                   
*                                                                               
GETREQ20 LA    R0,CARD                                                          
         PUT   REQOUT,(R0)         WRITE TO FILE                                
*                                                                               
         MVC   P(80),CARD                                                       
GETREQ22 GOTO1 =V(PRINTER)         PRINT OUT THE REQUEST CARD                   
         B     GETREQ                                                           
         DROP  Q                                                                
*                                                                               
GETREQX  DS    0H                                                               
         CLOSE (REQOUT,)                                                        
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
*                                                                               
ENDPROG  XBASE                                                                  
*                                                                               
GETPRD   L     R1,ADCLT                                                         
         AHI   R1,CLIST-CLTHDRD                                                 
*                                                                               
GETPRD2  CLC   SRTBPRD,3(R1)                                                    
         BER   RE                                                               
         AHI   R1,4                                                             
         CLI   0(R1),C'A'                                                       
         BNL   GETPRD2                                                          
         LA    R1,=C'***'                                                       
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
*=========================================================                      
*=========================================================                      
* DATAMGR CALLS                                                                 
*=========================================================                      
                                                                                
HIGH     LA    RF,=C'DMRDHI'                                                    
         MVC   KEYSAVE,KEY                                                      
         B     DIR                                                              
*                                                                               
SEQ      LA    RF,=C'DMRSEQ'                                                    
*                                                                               
DIR      NTR1                                                                   
         ST    RF,DMCB                                                          
         GOTO1 VDATAMGR,DMCB,,=C'SPTDIR',KEYSAVE,KEY                            
         B     EXIT                                                             
*                                                                               
GET      NTR1                                                                   
         GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'SPTFILE',KEY+14,AIO,DMWORK           
*                                                                               
*                                                                               
*                                                                               
EQXIT    CR    RB,RB               SET CC EQ                                    
         B     EXIT                                                             
NEQXIT   LTR   RB,RB               SET CC NEQ                                   
EXIT     XIT1                                                                   
         LTORG                                                                  
*                                                                               
*=========================================================                      
* USE DB PROFILE TO SEE IF CLIENT SHOULD BE OUTPUT                              
*=========================================================                      
                                                                                
CHKCLT   NTR1                                                                   
         L     R6,ADCLT                                                         
         CLC   SRTBAMCL,1(R6)      SAME A-M/CLT                                 
         BE    CHKCLT10                                                         
* READ NEW CLTHDR                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SRTBAMCL                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,ADCLT                                                        
         GOTO1 GET                                                              
*                                                                               
         L     R6,ADCLT            GET CORRECT EBCDIC CLIENT                    
         USING CLTHDRD,R6                                                       
         GOTO1 VCLUNPK,DMCB,(CPROF+6,CKEYCLT),CLIENT                            
         DROP  R6                                                               
*                                                                               
CHKCLT10 LA    R3,SORTREC                                                       
         USING REQD,R3                                                          
*                                                                               
         CLI   SVDBPROF,C' '       TEST NO CLIENT FILTER                        
         BNH   EQXIT                                                            
         CLI   SVDBPROF,C'*'       * MEANS NO FILTER TOO                        
         BE    EQXIT                                                            
*                                                                               
         L     R6,ADCLT                                                         
         USING CLTHDRD,R6                                                       
*                                                                               
         CLI   EXCLUDJ1,C'Y'       TEST TO EXCLUDE BY J1 OPTION                 
         BNE   CHKCLT12                                                         
         TM    COPT2,COP2EXDB      TEST EXCLUDE FROM J1 REQUEST                 
         BO    NEQXIT                                                           
*                                                                               
CHKCLT12 CLI   SVDBPROF,C'$'       TEST OFFICE LIST                             
         BNE   CHKCLT20                                                         
*                                                                               
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         XC    DUB,DUB                                                          
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,REQCLT                                                   
         MVC   OFCAGY,AGYSAVE                                                   
         MVC   OFCOFC,COFFICE                                                   
         DROP  R1                                                               
         GOTO1 VOFFICER,DMCB,DUB,(X'C0',ACOMFACS),SVOFCLST                      
                                                                                
         CLI   0(R1),0             TEST INCLUDE THIS CLIENT                     
         BNE   NEQXIT              NO                                           
         B     EQXIT                                                            
*                                                                               
CHKCLT20 CLI   SVDBPROF,C'O'       TEST OFFICE LIMIT                            
         BNE   CHKCLT30                                                         
*                                                                               
         CLC   SVDBPROF+1(1),COFFICE  SAME MEDIA OFFICE                         
         BNE   NEQXIT                                                           
         B     EQXIT                                                            
*                                                                               
CHKCLT30 CLI   SVDBPROF,C'G'          TEST CLIENT GROUP                         
         BNE   CHKCLT40                                                         
         DC    H'0'                <<<=== NOT CURRENTLY SUPPORTED               
*                                                                               
CHKCLT40 CLI   SVDBPROF,C'C'       TEST CLIENT                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   SVDBPROF+3,C'*'     MAKE A * INTO A SPACE                        
         BNE   *+8                                                              
         MVI   SVDBPROF+3,C' '                                                  
*                                                                               
         CLC   SVDBPROF+1(3),REQCLT                                             
         BNE   NEQXIT                                                           
         B     EQXIT                                                            
         DROP  R3,R6                                                            
*                                                                               
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,                        X        
               MACRF=GM,EODAD=ENDIN                                             
*                                                                               
REQOUT   DCB   DDNAME=REQOUT,DSORG=PS,RECFM=FB,LRECL=80,               X        
               BLKSIZE=3200,MACRF=PM                                            
*                                                                               
* SORT ON A/M-CLT/PRD/EST/MKT/STA                                               
SORTCARD DC   CL80'SORT FIELDS=(1,14,A,24,3,A,15,9,A),FORMAT=BI'                
RECCARD  DC   CL80'RECORD TYPE=F,LENGTH=32'                                     
*                                                                               
MDTAB    DC    C'TRNX***C'                                                      
*                                                                               
*                                                                               
         EJECT                                                                  
         DS    0D                                                               
*                                                                               
VSTAPACK DS    A                                                                
VCLUNPK  DS    A                                                                
VOFFICER DS    A                                                                
ACOMFACS DC    A(COMFACS)                                                       
VDATAMGR DC    V(DATAMGR)                                                       
VT00A9E  DS    A                                                                
*                                                                               
FULL     DS    F                                                                
HALF     DS    H                                                                
DATADISP DC    H'24'                                                            
BYTE     DS    X                                                                
FILSAVE  DS    CL6                                                              
ELCODE   DS    X                                                                
EXTRACTC DC    C' '                                                             
EXCLUDJ1 DC    C' '                                                             
*                                                                               
TRACEFLG DS    X                                                                
CTRYSAVE DS    X                                                                
AGYSAVE  DS    CL2                                                              
BAGY     DS    XL1                                                              
BAGYMD   DS    XL1                                                              
BCLT     DS    XL2                                                              
BPRD     DS    XL1                                                              
BMKTSTA  DS    0XL5                                                             
BMKT     DS    XL2                                                              
BSTA     DS    XL3                                                              
BEST     DS    XL1                                                              
CLIENT   DS    CL3                                                              
SVREQMED DS    C                                                                
*                                                                               
*                                                                               
         DS    0D                                                               
QSAVE    DS    XL26                                                             
         DS    XL6                 SPARE                                        
*                                                                               
         DS    0D                                                               
SVDBPROF DC    XL16'00'                                                         
SVOFCLST DS    CL48                                                             
SVBCLT   DS    XL2                                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'*SORTREC'                                                    
SORTREC  DS    XL32                                                             
         ORG   SORTREC                                                          
SRTQDATA DS    CL26                EBCDIC SORT DATA                             
*                                                                               
SRTBDATA DS    0XL6                BINARY A-M/CLT/PRD                           
SRTBAMCL DS    XL3                                                              
SRTBPRD  DS    XL1                                                              
SRTSPILL DS    CL1                                                              
SRTTYPE  DS    CL1  B=FROM BUY,G=FROM GOAL,P=PYBL,R=RCVBL                       
*                                                                               
CARD     DS    CL80                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'**UTL **'                                                    
UTL      DC    F'0',X'00'                                                       
*                                                                               
         DS    0D                                                               
         DC    CL8'**SSB **'                                                    
SSB      DC    256X'00'                                                         
         ORG   SSB                                                              
         DC    XL2'00',X'FF',X'02' NO RECOVERY                                  
         ORG                                                                    
*                                                                               
         DS    0D                                                               
         DC    C'*RECVREC'                                                      
RECVLEN  DS    H                   LOGICAL IOCS LEN                             
         DS    H                                                                
*PREFIX=DM$                                                                     
       ++INCLUDE DMRCVRHDR                                                      
*PREFIX=                                                                        
RDATA    DS    6100X                                                            
*                                                                               
*                                                                               
COMFACS  DS    0D                                                               
CDMGR    DC    V(DATAMGR)                                                       
CCALLOFF DC    V(CALLOFF)                                                       
         DC    200A(0)                                                          
*                                                                               
         DS    0D                                                               
STAWORK  DS    XL32                                                             
*                                                                               
SPTFLIST DC    CL8'NSPTFILE'                                                    
         DC    CL8'NSPTDIR '                                                    
         DC    CL8'NSTAFILE'                                                    
         DC    CL8'NCTFILE '                                                    
         DC    CL2'X '                                                          
*                                                                               
*                                                                               
REQD     DSECT                                                                  
REQTYP   DS    CL2   +0  COL 1     C'UB'/C'UF'                                  
REQAGY   DS    CL2   +2      3                                                  
REQMED   DS    CL1   +4      5                                                  
REQCLT   DS    CL3   +5      6                                                  
REQSRTNT DS    CL3                                                              
REQPRD   DS    CL3   +11    12     C'POL'                                       
REQMKT   DS    CL4   +14    15                                                  
REQSTA   DS    CL5   +18    19                                                  
REQEST   DS    CL3   +23    24                                                  
         DS    CL5                                                              
REQESDAT DS    CL2   +31    32     C'ES'                                        
         DS    CL10                                                             
REQCBNET DS    CL3   +43           CANADIAN CABLE NETWORK                       
         DS    CL15                                                             
REQOPT1  DS    CL1   +61    62     C'Y'                                         
REQOPT2  DS    CL1                 C'I'                                         
         DS    CL5                                                              
REQREQ   DS    CL12                                                             
REQDLQ   EQU   *-REQD                                                           
*                                                                               
*                                                                               
*                                                                               
SBGENWKD DSECT                                                                  
*                                                                               
* DOUBLEWORD-ALIGNED                                                            
*                                                                               
DUB      DS    D                                                                
KEY      DS    XL48                                                             
KEYSAVE  DS    XL48                                                             
*                                                                               
* FULLWORD-ALIGNED                                                              
*                                                                               
AIO      DS    A                                                                
ADCLT    DS    A                                                                
DMCB     DS    6F                                                               
DMWORK   DS    24F                                                              
*                                                                               
WORK     DS    XL64                                                             
CLTREC   DS    4096C                                                            
*                                                                               
SBGNWKLQ EQU   *-SBGENWKD                                                       
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE DDSYSELD                                                       
         EJECT                                                                  
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE DDOFFICED                                                      
SSBOFFD  DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPGENSTAB                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011SPSUBGEN  10/23/14'                                      
         END                                                                    
