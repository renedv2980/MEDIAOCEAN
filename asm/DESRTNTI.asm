*          DATA SET DESRTNTI   AT LEVEL 051 AS OF 12/15/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE DESRNTIA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE SORTER                                                                 
         TITLE 'POST-CONVERSION NATIONAL DEMOS FILE SORT'                       
DESRTNTI CSECT                                                                  
         PRINT NOGEN                                                            
         ENTRY SSB                                                              
         ENTRY UTL                                                              
*                                                                               
         NBASE 0,DESRTNTI,=V(REGSAVE)                                           
*                                                                               
         GOTO1 =V(STXITER),DMCB,STXTAB                                          
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(X'1E',TODAY14) TYPE-14 DATE               
         MVC   TITLE,=CL60'NATIONAL DEMOS POST-CONVERSION SORT JOB XXXX+        
               XXXX(JNNNNN)'                                                    
         LA    R2,FULL                                                          
         EXTRACT (2),'S',FIELDS=(ASID)                                          
         LA    R1,FULL                                                          
         L     R2,0(R1)                                                         
         LOCASCB ASID=(R2)         GET ASCB ADDRESS INTO R1                     
         L     R2,ASCBASSB-ASCB(R1) R2 = A(ASSB)                                
         SAM31 ,                   SWITCH TO 31-BIT MODE                        
         L     R1,ASSBJSAB-ASSB(R2) R1 = A(JSAB)                                
         MVC   TITLE+50(5),JSABJBID-JSAB+3(R1)  JOB#####                        
         SAM24 ,                   SWITCH BACK TO 24-BIT MODE                   
         LA    R2,FULL                                                          
         EXTRACT (2),FIELDS=TIOT                                                
         L     R2,FULL                                                          
         MVC   TITLE+40(8),0(R2)   JOBNAME                                      
*                                                                               
GETCARD  DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE10'  NOTE P2: NO SYSIN IS OKAY          
         CLC   =C'/*',CARD         EOF?                                         
         BE    OPENFIL                                                          
*                                                                               
         CLI   CARD,C'*'           COMMENT CARD?                                
         BE    GETCRD10            YES                                          
*                                                                               
         CLC   =C'DSPACE=',CARD                                                 
         BNE   *+18                                                             
         LA    RF,SSB                                                           
         MVC   SSODSPAC-SSOOFF(,RF),CARD+7   DSPACE= OVERRIDE                   
         B     GETCRD10                                                         
*                                                                               
         CLC   =C'QKEYS=',CARD     TRACE "Q" RECORD KEYS?                       
         BNE   GETCRD05                                                         
         CLI   CARD+6,C'Y'         QKEYS=Y ?                                    
         BE    GETCRD10            THAT'S THE DEFAULT                           
         CLI   CARD+6,C'N'         QKEYS=N ?                                    
         BE    *+6                                                              
         DC    H'0'                INVALID QKEYS= SUBOPTION                     
         MVI   PRNTQFLG,C'N'                                                    
         B     GETCRD10                                                         
*                                                                               
GETCRD05 DS    0H                                                               
         CLC   =C'JKEYS=',CARD     TRACE "J" RECORD KEYS?                       
         BNE   GETCRD10                                                         
         CLI   CARD+6,C'Y'         JKEYS=Y ?                                    
         BE    GETCRD10            THAT'S THE DEFAULT                           
         CLI   CARD+6,C'N'         JKEYS=N ?                                    
         BE    *+6                                                              
         DC    H'0'                INVALID JKEYS= SUBOPTION                     
         MVI   PRNTJFLG,C'N'                                                    
*                                                                               
GETCRD10 DS    0H                                                               
         B     GETCARD             READ NEXT PARAMETER CARD                     
*                                                                               
OPENFIL  DS    0H                                                               
         OPEN  FILIN1                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R0,IOSRTKLN         SOFT SORT KEY LENGTH                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCRD+15(2),DUB                                                
         GOTO1 =V(SORTER),DMCB,SORTCRD,(X'80',RECCRD),(X'80',0)                 
*                                                                               
READREC  LA    R2,IOAREA                                                        
         USING IOD,R2                                                           
         XC    IOLEN(IOSRTLN),IOLEN  CLEAR SORT KEY TO BUILD                    
         GET   FILIN1,IOACTLN      ACTUAL RECD START IN IOAREA                  
         AP    INCNT,=P'1'         BUMP INPUT RECORD COUNT                      
*                                                                               
         MVC   IOLEN,IOACTLN       COPY RDW                                     
         MVI   IOSRTKEY,X'FF'      ASSUME NOT AN NTI BITMAP RECORD              
*                                                                               
         CLC   =AL2(PASSIVLN),IOACTLN   PASSIVE KEY?                            
         BNE   *+14                NO                                           
         MVC   IOKEYMAJ,IOACTKEY   YES: ONLY USE MAJOR KEY FOR SORT KEY         
         B     PUT2SORT                                                         
*                                                                               
         MVC   IOKEY,IOACTKEY      MAJOR AND MINOR KEY                          
*                                                                               
PI       USING PRKEY,IOKEYMAJ                                                   
         CLC   PI.PRKEY(3),=C'PNN' IS IT AN NTI BITMAP RECORD?                  
         BNE   SORT3                                                            
         CLC   PI.PRSTAT(4),=C'PPPP'                                            
         BNE   SORT3                                                            
         MVI   IOSRTKEY,0          YES: FORCE THESE FIRST IN SORT               
         MVI   BITMPFLG,C'Y'       REMEMBER THAT WE HAD ONE OF THESE            
         DROP  PI                                                               
*                                                                               
SORT3    DS    0H                                                               
         CLI   IOKEYMAJ,PMCODEQU   FOR Q-RECDS ONLY, SORT ON DAY                
         BNE   PUT2SORT                                                         
         LA    R1,IODATA           PT TO 1ST ELEMENT IN RECD                    
*                                                                               
SORT4    CLI   0(R1),0             EOR?                                         
         BE    PUT2SORT                                                         
         CLI   0(R1),NTCODEQU      RUN TIME ELEMENT?                            
         USING NTELEM,R1                                                        
         BE    SORT4A              YES                                          
         LLC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     SORT4                                                            
*                                                                               
SORT4A   DS    0H                                                               
         MVC   BYTE,NTDAY                                                       
         DROP  R1                                                               
*                                                                               
         LA    R1,DAYTAB           CONVERT BITS TO DAY CODE                     
SORT4B   CLC   BYTE,0(R1)          COMPARE BIT SETTING                          
         BE    *+8                                                              
         CLI   0(R1),X'FF'         END OF TABLE?                                
         BE    *+12                                                             
         LA    R1,L'DAYTAB(R1)     NEXT ENTRY IN TABLE                          
         B     SORT4B                                                           
         MVC   IOSRTDAY,1(R1)      SAVE DAY CODE IN KEY                         
*                                                                               
PUT2SORT DS    0H                                                               
         SR    R1,R1                                                            
         ICM   R1,3,IOLEN                                                       
         LA    R1,IOSRTLN(R1)      ADJUST LENGTH OF RECD                        
         STCM  R1,3,IOLEN          SAVE NEW LENGTH                              
         GOTO1 =V(SORTER),DMCB,=C'PUT',IOAREA                                   
*                                                                               
         B     READREC                                                          
*                                                                               
CLOSEIN  DS    0H                                                               
         CLOSE (FILIN1,)                                                        
*                                                                               
         CLI   BITMPFLG,C'Y'                                                    
         BNE   OPENOUT                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
E        USING FDTELEM,ELEM                                                     
         MVI   E.FDTCODE,FDTCODEQ                                               
         MVI   E.FDTLEN,FDTLENEQ                                                
         GOTO1 =V(DATCON),DMCB,(5,0),(3,E.FDTLDDTE)                             
         DROP  E                                                                
*                                                                               
         XC    IOAREA(PASSIVLN+IOSRTLN+FDTLENEQ+1),IOAREA                       
         MVC   IOLEN(2),=AL2(PASSIVLN+IOSRTLN+FDTLENEQ+1)                       
         MVI   IOSRTKEY,1          FORCE THIS RECORD RIGHT AFTER BITMAP         
         MVC   IOACTLN(2),=AL2(PASSIVLN+FDTLENEQ+1)                             
         MVI   IOACTKEY+0,DUCODEQU DUMMY RECORD KEY                             
         MVI   IOACTKEY+1,DUMEDEQU                                              
         MVI   IOACTKEY+2,DUSRCEQU                                              
         MVC   IOACTLEN,=AL2(PASSIVLN-4+FDTLENEQ+1)                             
         LHI   R1,FDTLENEQ                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IODATA(0),ELEM                                                   
         GOTO1 =V(SORTER),DMCB,=C'PUT',IOAREA                                   
         AP    INCNT,=P'1'         BUMP INPUT RECORD COUNT                      
         DROP  R2                                                               
*                                                                               
OPENOUT  DS    0H                                                               
         OPEN  (FILOUT,(OUTPUT))                                                
*                                                                               
GETSORT  GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R3,15,DMCB+4        A(SORT RECORD)                               
         BZ    ENDSORT             NO MORE RECORDS                              
*                                                                               
         USING IOD,R3                                                           
         CLC   =AL2(PASSIVLN),IOACTLN   PASSIVE KEY?                            
         BNE   KEEPIT              NO: KEEP THE RECORD                          
         CLC   SVPASVKY,IOACTKEY   DOES THIS KEY MATCH PREVIOUS?                
         BE    *+16                YES: SKIP THIS DUPLICATE                     
         CLC   IOACTKEY,SVPASVKY   MAJOR KEY MATCHES PREV. MAJOR KEY?           
         BNE   KEEPIT              NO: IT'S A NEW ONE. KEEP IT.                 
         DC    H'0'                YES: SHOULD NEVER HAPPEN                     
*                                                                               
         AP    SKIPDUP,=P'1'       BUMP SKIPPED RECORD COUNT                    
         B     GETSORT             DON'T WRITE OUT DUPLICATE PASSIVES           
*                                                                               
KEEPIT   DS    0H                                                               
         MVC   SVPASVKY,IOACTKEY   REMEMBER THIS MAJOR KEY                      
         DROP  R3                                                               
*                                                                               
         LA    R3,IOSRTLN(R3)      BUMP PAST SORT KEY FIELDS                    
         PUT   FILOUT,(R3)                                                      
*                                                                               
         LA    R5,4(R3)            BUMP PAST RDW                                
         USING PMKEY,R5                                                         
*                                                                               
         CLI   PMCODE,PMCODEQU     "Q" RECORD?                                  
         BNE   CHKZKEY                                                          
         CLI   PMMEDIA,C'N'        LOOKING FOR QNN KEYS                         
         BNE   NEXTREC                                                          
         CLI   PMSRC,C'N'                                                       
         BNE   NEXTREC                                                          
*                                                                               
         CLI   PRNTQFLG,C'Y'       PRINT THE "Q" KEYS?                          
         BNE   NEXTREC             NO                                           
         CLC   SVMAJKEY,0(R5)      CHANGE OF PARTIAL MAJOR KEY?                 
         BE    NEXTREC             NO: DON'T PRINT IT                           
*                                                                               
*                                  PRINT THE KEY IN SYSPRINT TRACE              
         MVC   P(6),=C'QKEY: '                                                  
         MVC   P+6(1),PMCODE       "Q"                                          
         MVC   P+7(1),PMMEDIA      MEDIA                                        
         MVC   P+8(1),PMSRC        SOURCE                                       
         MVC   P+10(5),PMSTAT      STATION                                      
         MVC   P+16(2),=C'20'      *Y2K* ASSUME 21ST CENTURY                    
         EDIT  (B1,PMBOOK),(2,P+18),FILL=0    BOOK YEAR                         
         MVI   P+20,C'/'                                                        
         EDIT  (B1,PMBOOK+1),(2,P+21),FILL=0  BOOK WEEK                         
         MVC   P+24(1),PMSTYP      STATION TYPE                                 
         MVC   P+26(1),PMBTYP      BOOK TYPE                                    
         GOTO1 =V(HEXOUT),DMCB,0(R5),P+63,L'PMKMAJOR,=C'TOG'                    
         OC    DMCB+16(4),DMCB+16                                               
         JZ    *+2                                                              
         GOTO1 =V(PRINTER)                                                      
         MVC   SVMAJKEY,0(R5)      SAVE THE PARTIAL "Q" KEY                     
         B     NEXTREC                                                          
         DROP  R5                                                               
*                                                                               
CHKZKEY  DS    0H                                                               
         CLI   PRNTJFLG,C'Y'       PRINT THE "J" AND "Z" KEYS?                  
         BNE   NEXTREC             NO                                           
*                                                                               
         LA    R5,4(R3)            BUMP PAST RDW                                
         USING PZKEY,R5                                                         
*                                                                               
         CLI   PZCODE,PZCODEQU     "Z" PASSIVE?                                 
         BNE   CHKJKEY                                                          
         CLI   PZMEDIA,C'N'        LOOKING FOR ZNN KEYS...                      
         BNE   NEXTREC                                                          
         CLI   PZSRC,C'N'                                                       
         BNE   NEXTREC                                                          
*                                                                               
         CLC   PZUPFDAT,TODAY14    ...THAT WERE ADDED TODAY...                  
         BNE   *+14                                                             
         MVC   P+55(5),=C'ADDED'                                                
         B     *+20                                                             
         CLC   PZUPLDAT,TODAY14    ...OR UPDATED TODAY                          
         BNE   NEXTREC                                                          
         MVC   P+55(7),=C'UPDATED'                                              
*                                                                               
         MVC   P(6),=C'ZKEY: '                                                  
         B     PRTZKEY                                                          
         DROP  R5                                                               
*                                                                               
CHKJKEY  DS    0H                                                               
         USING PJKEY,R5                                                         
         CLI   PJCODE,PJCODEQU     "J" RECORD?                                  
         BNE   NEXTREC                                                          
         CLI   PJMEDIA,C'N'        LOOKING FOR JNNPPPP KEYS                     
         BNE   NEXTREC                                                          
         CLI   PJSRC,C'N'                                                       
         BNE   NEXTREC                                                          
         CLC   =C'PPPP',PJSTAT                                                  
         BNE   NEXTREC                                                          
         OC    PJBOOK,PJBOOK       NO BOOK = BITMAP "J" KEYS                    
         BNZ   NEXTREC                                                          
         MVC   P(6),=C'JKEY: '                                                  
*                                                                               
PRTZKEY  DS    0H                                                               
         USING PZKEY,R5                                                         
*                                                                               
* THE "J" AND "Z" KEYS ARE STRUCTURED ALMOST IDENTICALLY. THAT'S WHY            
* WE CAN USE THE SAME CODE HERE TO TRACE BOTH FLAVORS IN SYSPRINT               
* (I.E., THAT'S WHY WE CAN USE THE PZKEY DSECT FOR PJKEY).                      
*                                                                               
         MVC   P+6(1),PZCODE                                                    
         MVC   P+7(1),PZMEDIA                                                   
         MVC   P+8(1),PZSRC                                                     
         MVC   P+9(5),PZSTAT                                                    
         MVC   P+15(10),=C'NIELSEN#: '                                          
         GOTO1 =V(HEXOUT),DMCB,PZEXTNUM,P+25,L'PZEXTNUM,=C'TOG'                 
         OC    16(4,R1),16(R1)                                                  
         JZ    *+2                                                              
         MVC   P+38(10),=C'DDS NTI#: '                                          
         SR    R1,R1                                                            
         ICM   R1,B'0011',PZINTNUM                                              
         EDIT  (R1),(5,P+48),ZERO=NOBLANK                                       
         GOTO1 =V(HEXOUT),DMCB,0(R5),P+63,L'PZKMAJOR,=C'TOG'                    
         OC    DMCB+16(4),DMCB+16                                               
         JZ    *+2                                                              
         GOTO1 =V(PRINTER)                                                      
         DROP  R5                                                               
*                                                                               
NEXTREC  DS    0H                                                               
         AP    OUTCNT,=P'1'        BUMP OUTPUT RECORD COUNT                     
         B     GETSORT                                                          
*                                                                               
ENDSORT  DS    0H                                                               
         CLOSE FILOUT                                                           
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
*                                                                               
         MVC   P(23),=C'SORTER INPUT RECORDS:  '                                
         EDIT  INCNT,(10,P+23),ZERO=NOBLANK                                     
         GOTO1 =V(PRINTER)                                                      
         MVC   P(23),=C'SORTER OUTPUT RECORDS: '                                
         EDIT  OUTCNT,(10,P+23),ZERO=NOBLANK                                    
         GOTO1 =V(PRINTER)                                                      
         MVC   P(23),=C'SKIPPED DUP. PASSIVES: '                                
         EDIT  SKIPDUP,(10,P+23),ZERO=NOBLANK                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         AP    OUTCNT,SKIPDUP      #KEPT + #SKIPPED...                          
         CP    INCNT,OUTCNT        ...HAD BETTER = #IN !                        
         BE    XBASE                                                            
         MVC   P(40),=C'*** ERROR: SORTER RECORD COUNT MISMATCH!'               
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                WHAT GOES IN MUST COME OUT!                  
*                                                                               
XBASE    DS    0H                                                               
         XBASE ,                                                                
         EJECT                                                                  
DAYTAB   DS    0XL2                       DAY CONVERSION TABLE                  
         DC    X'7C',X'00'         M-F        SORT FIRST                        
         DC    X'7F',X'01'         M-S        SORT AFTER M-F                    
         DC    X'40',X'10'         MON                                          
         DC    X'20',X'20'         TUE                                          
         DC    X'10',X'30'         WED                                          
         DC    X'08',X'40'         THU                                          
         DC    X'04',X'50'         FRI                                          
         DC    X'02',X'60'         SAT                                          
         DC    X'01',X'70'         SUN                                          
         DC    X'FF',X'02'         VAR       SORT AFTER M-S  MISC DAYS          
*                                                                               
SORTCRD  DC    CL80'SORT FIELDS=(5,XX,A),FORMAT=BI'                             
RECCRD   DC    CL80'RECORD TYPE=V,LENGTH=2100'                                  
*                                                                               
         EJECT                                                                  
FILIN1   DCB   DDNAME=FILIN1,DSORG=PS,RECFM=VB,MACRF=(GM),             X        
               EODAD=CLOSEIN,LRECL=2000,BLKSIZE=0                               
FILOUT   DCB   DDNAME=FILOUT,DSORG=PS,RECFM=VB,MACRF=(PM),             X        
               LRECL=2000,BLKSIZE=8200                                          
*                                                                               
STXTAB   DS    0H                                                               
         DC    A(DESRTNTI)                                                      
         DC    V(DUMMY)                                                         
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
DMCB     DS    6F                                                               
INCNT    DC    PL8'0'                                                           
OUTCNT   DC    PL8'0'                                                           
SKIPDUP  DC    PL8'0'                                                           
TODAY14  DS    XL2                 TODAY'S DATE (TYPE-14)                       
WORK     DS    CL17                                                             
BYTE     DS    X                                                                
PRNTQFLG DC    C'Y'                                                             
PRNTJFLG DC    C'Y'                                                             
BITMPFLG DC    C'N'                                                             
SVPASVKY DC    XL23'00'            SAVE PREVIOUS PASSIVE KEY                    
SVMAJKEY DC    XL(PMOPIID-PMKEY)'00'     SAVE "Q" KEY UP TO BOOKTYPE            
ELEM     DS    XL255                                                            
CARD     DS    CL80                                                             
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
UTL      DS    0D                                                               
         DC    4X'00',X'0C'        UTL FOR DEMO SYSTEM                          
         SPACE 2                                                                
         DS    0D                                                               
         DC    CL16'******SSB*******'                                           
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   SSOMTIND            SYSTEM DATAMGR FLAGS                         
         DC    AL1(SSOWRTN)        WRITE=NO (DON'T OPEN FOR UPDATE)             
         ORG                                                                    
         EJECT                                                                  
         DS    0D                                                               
         DC    CL32'****** DESRTNTI I/O AREA *******'                           
IOAREA   DS    CL5000                                                           
         EJECT                                                                  
IOD      DSECT ,                                                                
IOLEN    DS    XL4                 RDW                                          
IOSRTKEY DS    X                   TO FORCE NTI BITMAP FIRST                    
IOKEY    DS    0XL20                                                            
IOKEYMAJ DS    CL18                MAJOR KEY                                    
IOKEYMIN DS    XL2                 MINOR KEY                                    
IOSRTDAY DS    X                   SORT Q-RECDS ON DAY                          
IOSRTKLN EQU   *-IOSRTKEY                                                       
IOSRTLN  EQU   *-IOD                                                            
IOACTLN  DS    XL4                 ACTUAL RECD RDW                              
IOACTKEY DS    CL18                MAJOR KEY                                    
IOACTMIN DS    XL2                 MINOR KEY                                    
IOACTLEN DS    XL2                 LENGTH                                       
IOACTSTA DS    X                   STATUS                                       
PASSIVLN EQU   *-IOACTLN           L'PASSIVE KEY                                
IODATA   DS    0C                  START OF RECD'S ELEMS                        
*                                                                               
         EJECT                                                                  
* ++INCLUDE DDDPRINT                                                            
* ++INCLUDE DEDEMFILE                                                           
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 3                                                                
* REDEFINE THE LAST 5 BYTES OF THE "Z" EXTENDED PASSIVE KEY. THIS IS            
* HOW THE RECORDS APPEAR IN THE CONVERSION OUTPUT FILE.                         
*                                                                               
PZKEY    DSECT                                                                  
         ORG   PZSTATUS                                                         
PZUPFDAT DS    XP(DT14)L2          FIRST LOAD DATE FOR THIS NIELSEN#            
PZUPLDAT DS    XP(DT14)L2          LATEST LOAD DATE FOR THIS NIELSEN#           
PZUPSTAT DS    X                   STATUS BYTE FOR UPDATE INPUT                 
         ORG                                                                    
         SPACE 3                                                                
*                                                                               
         IHAASCB                                                                
         IHAASSB                                                                
         IAZJSAB                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051DESRTNTI  12/15/20'                                      
         END                                                                    
