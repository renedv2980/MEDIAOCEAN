*          DATA SET SPREPMO02  AT LEVEL 012 AS OF 08/28/02                      
*PHASE SPMO02A                                                                  
         TITLE 'SPMO02 - COPY ADWARE DATA TO WORKER FILE'                       
SPMO02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPMO02,R8,RR=R2                                                
         ST    R2,RELO                                                          
                                                                                
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
                                                                                
         CLI   MODE,REQFRST                                                     
         BE    MO10                                                             
         B     EXIT                                                             
                                                                                
RELO     DC    A(0)                                                             
                                                                                
MO10     DS    0H                                                               
         CLC   QAREA+49(6),SPACES  IS THERE A LINE # TO START AT?               
         BNE   MO20                                                             
         OI    RECFLAG,FNDLINE     NO, SO DON'T WORRY ABOUT IT                  
         B     MO25                                                             
                                                                                
MO20     DS    0H                                                               
         PACK  DUB,QAREA+49(6)                                                  
         CVB   R1,DUB                                                           
         ST    R1,STARTLN                                                       
                                                                                
MO25     DS    0H                                                               
         CLC   QAREA+55(6),SPACES  IS THERE A LINE # TO END  AT?                
         BE    MO27                                                             
         PACK  DUB,QAREA+55(6)                                                  
         CVB   R1,DUB                                                           
         ST    R1,LASTLN                                                        
                                                                                
MO27     DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(1,TDAY)                                       
         BAS   RE,WRKROPEN                                                      
         OPEN  (TIN,INPUT)                    OPEN UPLOAD                       
         LA    R2,IO                                                            
                                                                                
MO30     DS    0H                                                               
         ZICM  R1,LASTLN,4                                                      
         BZ    *+12                                                             
         C     R1,LINENUM                                                       
         BNH   MOX                                                              
                                                                                
         GET   TIN,(R2)                                                         
         L     R1,LINENUM                                                       
         LA    R1,1(R1)                                                         
         ST    R1,LINENUM                                                       
                                                                                
         TM    RECFLAG,FNDLINE     NO, SO DON'T WORRY ABOUT IT                  
         BO    MO40                                                             
         L     R1,STARTLN                                                       
         C     R1,LINENUM                                                       
         BNE   MO30                                                             
                                                                                
         MVC   STRTLN,STARTLN                                                   
         OI    RECFLAG,FNDLINE                                                  
         CLC   0(3,R2),=C'HDR'     HEADER                                       
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
MO40     DS    0H                                                               
         ZICM  R1,LASTLN,4                                                      
         BZ    *+12                                                             
         C     R1,LINENUM                                                       
         BL    MOX                                                              
                                                                                
         LA    R3,IO2+10                                                        
         LA    R1,IO2L                                                          
         XC    0(14,R1),0(R1)                                                   
                                                                                
         CLC   0(3,R2),=C'HDR'     HEADER                                       
         BE    MOHDR                                                            
         CLC   0(3,R2),=C'BUY'     BUY                                          
         BE    MOBUY                                                            
         CLC   0(3,R2),=C'SKD'     SCHEDULE                                     
         BE    MOSKD                                                            
         CLC   0(3,R2),=C'ROT'     ROTATIONS                                    
         BE    MOROT                                                            
         CLC   0(3,R2),=C'DEM'     DEMO VALUES                                  
         BE    MODEM                                                            
         CLC   0(3,R2),=C'COM'     COMMENTS                                     
         BE    MOCOM                                                            
         B     MO30                                                             
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                        HEADER                                       *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
MOHDR    DS    0H                                                               
*&&DO                                                                           
         LA    R2,80(R2)                                                        
         GET   TIN,(R2)                                                         
         LA    R2,80(R2)                                                        
         GET   TIN,(R2)                                                         
         LA    R2,IO                                                            
         L     R1,LINENUM                                                       
         LA    R1,2(R1)                                                         
         ST    R1,LINENUM                                                       
*&&                                                                             
                                                                                
         CLI   LASTREC,C'B'                                                     
         BE    MOHDR10                                                          
         CLI   LASTREC,C'S'                                                     
         BE    MOHDR10                                                          
         CLI   LASTREC,C'R'                                                     
         BE    MOHDR10                                                          
         CLI   LASTREC,C'D'                                                     
         BE    MOHDR10                                                          
         CLI   LASTREC,C'C'                                                     
         BNE   *+8                                                              
                                                                                
MOHDR10  DS    0H                                                               
         BAS   RE,EOB                                                           
         LA    R1,300                                                           
         C     R1,SEQNUM           IF AT LEAST 300 RECORDS,                     
         BH    *+12                 OPEN ANOTHER WORKER FILE                    
         BAS   RE,WRKRCLSE                                                      
         BAS   RE,WRKROPEN                                                      
                                                                                
         MVI   LASTREC,C'H'                                                     
         NI    RECFLAG,X'FF'-BUYERR-HDRERR                                      
         USING SHDRD,R3                                                         
         XC    SHDRERNO(SHDRTYPE-SHDRODAT),SHDRERNO                             
         MVC   SHDRLEN,=AL2(SHDRLENQ)                                           
         MVI   SHDRSTRT,C' '       PAD WITH SPACES                              
         MVC   SHDRSTRT+1(SHDRRLNQ-1),SHDRSTRT                                  
         LA    R1,SHDRLENQ                                                      
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   SHDRTYPE(0),0(R2)                                                
                                                                                
         CLC   SHDRPRD,=C'COC'                                                  
         BNE   *+10                                                             
         MVC   SHDRPRD,=C'CO '                                                  
                                                                                
         CLI   SHDRMED,C'C'        ADWARE SCREWED UP                            
         BNE   *+8                                                              
         MVI   SHDRMED,C'T'                                                     
                                                                                
MOHDR20  DS    0H                                                               
         LA    R6,14                                                            
         LA    R5,SHDRDEMO                                                      
MOHDR30  DS    0H                                                               
         CLC   1(6,R5),=C'AD1234'                                               
         BNE   *+10                                                             
         MVC   1(6,R5),=C'V1234 '                                               
         LA    R5,L'SHDRDEMO(R5)                                                
         BCT   R6,MOHDR30                                                       
                                                                                
         MVC   SHDRUPDO,QAREA+61                                                
         MVC   IO2L(2),=Y(SHDRLENQ+16)                                          
         BAS   RE,WRKR                                                          
         B     MO30                                                             
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                             BUY                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
MOBUY    DS    0H                                                               
*&&DO                                                                           
         LA    R2,80(R2)                                                        
         GET   TIN,(R2)                                                         
         L     R1,LINENUM                                                       
         LA    R1,1(R1)                                                         
         ST    R1,LINENUM                                                       
         LA    R2,IO                                                            
*&&                                                                             
                                                                                
         MVC   NEXTALPH,=A(ALPHABET)                                            
         CLI   LASTREC,C'H'                                                     
         BE    *+8                                                              
         BAS   RE,EOB                                                           
         MVI   LASTREC,C'B'                                                     
         TM    RECFLAG,HDRERR                                                   
         BO    MO30                                                             
                                                                                
         NI    RECFLAG,X'FF'-BUYERR                                             
         USING SBUYD,R3                                                         
         XC    SBUYERNO(SBUYTYPE-SBUYODAT),SBUYERNO                             
         MVC   SBUYLEN,=AL2(SBUYLENQ)                                           
         MVI   SBUYSTRT,C' '       PAD WITH SPACES                              
         MVC   SBUYSTRT+1(SBUYRLNQ-1),SBUYSTRT                                  
         LA    R1,SBUYLENQ                                                      
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   SBUYTYPE(0),0(R2)                                                
                                                                                
*                  CHECK FOR LOW POWER STATION CONVERSION                       
         LA    RE,LOWSTAB          TABLE OF LOW POWER STATIONS                  
MOBUY10  CLI   0(RE),X'FF'                                                      
         BE    MOBUY20             STATION NOT IN TABLE                         
         CLC   SBUYSTA(4),0(RE)    MATCH ON 4                                   
         BNE   *+14                                                             
         MVC   SBUYSTA,4(RE)       REPLACE WITH 8                               
         B     MOBUY20                                                          
         LA    RE,12(RE)                                                        
         B     MOBUY10                                                          
                                                                                
MOBUY20  CLI   SBUYDPT,C'I'                                                     
         BNE   *+8                                                              
         MVI   SBUYDPT,C'P'                                                     
                                                                                
         LA    R4,L'SBUYPROG       GET RID OF ALL COMMAS                        
         LA    R5,SBUYPROG                                                      
MOBUY50  DS    0H                                                               
         CLI   0(R5),C','                                                       
         BNE   *+8                                                              
         MVI   0(R5),C' '                                                       
         LA    R5,1(R5)                                                         
         BCT   R4,MOBUY50                                                       
                                                                                
         CLC   SBUYCOST,SPACES                                                  
         BNE   *+14                                                             
         MVC   P2+PRERRMSG-PRERRHD(11),=C'NO BUY COST'                          
         B     BUYERROR                                                         
                                                                                
* SHIFT UNIQUE BUY ID LEFT ONE BYTE                                             
         CLI   SBUYUID,C'0'        CHECK IF DIGIT DELETING IS A ZERO            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SBUYUID(L'SBUYUID-1),SBUYUID+1                                   
         MVI   SBUYUID+L'SBUYUID-1,C' '                                         
                                                                                
         MVC   IO2L(2),=Y(SBUYLENQ+16)                                          
         BAS   RE,WRKR                                                          
         MVC   SVBUY,IO2L                                                       
         B     MO30                                                             
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                        SCHEDULE                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
MOSKD    DS    0H                                                               
         USING SSKDD,R3                                                         
         TM    RECFLAG,PROCSKD     ALREADY PROCESSED                            
         BZ    *+14                                                             
         MVC   SSKDLEN(L'SVSKD),SVSKD                                           
         B     MOSKD40                                                          
                                                                                
         MVI   LASTREC,C'S'                                                     
         TM    RECFLAG,HDRERR+BUYERR                                            
         BNZ   MO30                                                             
         XC    SSKDERNO(SSKDTYPE-SSKDODAT),SSKDERNO                             
         MVC   SSKDLEN,=AL2(SSKDLENQ)                                           
         MVI   SSKDSTRT,C' '       PAD WITH SPACES                              
         MVC   SSKDSTRT+1(SSKDLNQ-1),SSKDSTRT                                   
         LA    R1,SSKDLENQ                                                      
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   SSKDTYPE(0),0(R2)                                                
                                                                                
* MAKE A TABLE OF SKD PACKED                                                    
         LA    RE,SSKDCNTR                                                      
         LA    RF,PSKDTAB                                                       
         LA    R6,14               14 SKD VALUES                                
MOSKD10  DS    0H                                                               
         SR    R1,R1               FIGURE OUT LENGTH                            
         LR    R4,RE                                                            
         LA    R5,L'SSKDCNTR                                                    
                                                                                
MOSKD20  CLI   0(R4),C' '                                                       
         BE    MOSKD30                                                          
         LA    R1,1(R1)                                                         
         LA    R4,1(R4)                                                         
         BCT   R5,MOSKD20                                                       
                                                                                
MOSKD30  DS    0H                                                               
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  0(L'PSKDTAB,RF),0(0,RE)                                          
         LA    RE,L'SSKDCNTR(RE)                                                
         LA    RF,L'PSKDTAB(RF)                                                 
         BCT   R6,MOSKD10                                                       
                                                                                
* NOW MAKE SURE THERE ARE NO MORE THAN 4 SPOTS/WEEK                             
MOSKD40  DS    0H                                                               
         NI    RECFLAG,X'FF'-PROCSKD                                            
         LA    R4,PSKDTAB                                                       
         LA    R5,14               14 SKD VALUES                                
         LA    R6,SSKDCNTR                                                      
                                                                                
MOSKD50  DS    0H                                                               
         ZAP   PSKD,0(L'PSKDTAB,R4)                                             
         SP    PSKD,=P'4'                                                       
         BNM   MOSKD60                                                          
         UNPK  0(L'SSKDCNTR,R6),0(L'PSKDTAB,R4)                                 
         ZAP   0(L'PSKDTAB,R4),=P'0'                                            
         B     MOSKD70                                                          
                                                                                
MOSKD60  DS    0H                                                               
         BZ    *+8                                                              
         OI    RECFLAG,PROCSKD     MORE THAN 4 SO MAKE ANOTHER BUY              
         ZAP   0(L'PSKDTAB,R4),PSKD                                             
         UNPK  0(L'SSKDCNTR,R6),=PL2'4'                                         
                                                                                
MOSKD70  DS    0H                                                               
         OI    L'SSKDCNTR-1(R6),X'F0'                                           
         LA    R6,L'SSKDCNTR(R6)                                                
         LA    R4,L'PSKDTAB(R4)                                                 
         BCT   R5,MOSKD50                                                       
                                                                                
         MVC   IO2L(2),=Y(SSKDLENQ+16)                                          
         BAS   RE,WRKR                                                          
         MVC   SVSKD,SSKDLEN                                                    
         TM    RECFLAG,PROCSKD2                                                 
         BNO   MO30                                                             
         NI    RECFLAG,X'FF'-PROCSKD2                                           
         B     MODEM10                                                          
         DROP  R3                                                               
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                        ROTATIONS                                              
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
MOROT    DS    0H                                                               
         MVI   LASTREC,C'R'                                                     
         TM    RECFLAG,HDRERR+BUYERR                                            
         BNZ   MO30                                                             
         USING SROTD,R3                                                         
         XC    SROTERNO(SROTTYPE-SROTODAT),SROTERNO                             
         MVC   SROTLEN,=AL2(SROTLENQ)                                           
         MVI   SROTSTRT,C' '       PAD WITH SPACES                              
         MVC   SROTSTRT+1(SROTLNQ-1),SROTSTRT                                   
         LA    R1,SROTLENQ                                                      
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   SROTTYPE(0),0(R2)                                                
                                                                                
         MVC   IO2L(2),=Y(SROTLENQ+16)                                          
         BAS   RE,WRKR                                                          
         B     MO30                                                             
                                                                                
         DROP  R3                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                        DEMOS                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
MODEM    DS    0H                                                               
*&&DO                                                                           
         LA    R2,80(R2)                                                        
         GET   TIN,(R2)                                                         
         LA    R2,IO                                                            
         L     R1,LINENUM                                                       
         LA    R1,1(R1)                                                         
         ST    R1,LINENUM                                                       
*&&                                                                             
                                                                                
         MVI   LASTREC,C'D'                                                     
         TM    RECFLAG,HDRERR+BUYERR                                            
         BNZ   MO30                                                             
         USING SDEMD,R3                                                         
                                                                                
MODEM10  DS    0H                                                               
         XC    SDEMERNO(SDEMTYPE-SDEMODAT),SDEMERNO                             
         MVC   SDEMLEN,=AL2(SDEMLENQ)                                           
         MVI   SDEMSTRT,C' '       PAD WITH SPACES                              
         MVC   SDEMSTRT+1(SDEMRLNQ-1),SDEMSTRT                                  
         LA    R1,SDEMLENQ                                                      
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   SDEMTYPE(0),0(R2)                                                
                                                                                
         MVC   IO2L(2),=Y(SDEMLENQ+16)                                          
         BAS   RE,WRKR                                                          
                                                                                
         TM    RECFLAG,PROCSKD     CHECK IF NEED TO MAKE ANOTHER BUY            
         BNO   MO30                NO                                           
         BAS   RE,EOB                                                           
         MVC   IO2L(L'SVBUY),SVBUY                                              
                                                                                
         L     R1,NEXTALPH         ADDRESS OF NEXT LETTER TO USE                
         CLI   0(R1),X'FF'           FOR UNIQUE BUY ID                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   IO2+10+SBUYUID-SBUYD+L'SBUYUID-1(1),0(R1)                        
         LA    R1,1(R1)                                                         
         ST    R1,NEXTALPH                                                      
                                                                                
         BAS   RE,WRKR                                                          
         OI    RECFLAG,PROCSKD2                                                 
         B     MOSKD                                                            
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                        COMMENTS                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
MOCOM    DS    0H                                                               
         MVI   LASTREC,C'C'                                                     
         TM    RECFLAG,HDRERR+BUYERR                                            
         BNZ   MO30                                                             
         USING SCOMD,R3                                                         
         XC    SCOMERNO(SCOMTYPE-SCOMODAT),SCOMERNO                             
         MVC   SCOMLEN,=AL2(SCOMLENQ)                                           
         MVI   SCOMSTRT,C' '       PAD WITH SPACES                              
         MVC   SCOMSTRT+1(SCOMLNQ-1),SCOMSTRT                                   
         LA    R1,SCOMLENQ                                                      
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   SCOMTYPE(0),0(R2)                                                
                                                                                
         MVC   IO2L(2),=Y(SCOMLENQ+16)                                          
         BAS   RE,WRKR                                                          
         B     MO30                                                             
                                                                                
         DROP  R3                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                           EXIT                                      *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
MOX      DS    0H                                                               
         BAS   RE,EOB                                                           
         LA    R1,5                PURGE IF NO NEW ENTRIES                      
         C     R1,SEQNUM                                                        
         BL    *+8                                                              
         OI    RECFLAG,PRGWRKR     DELETE LAST WORKER FILE                      
         BAS   RE,WRKRCLSE                                                      
         TM    RECFLAG,PRGWRKR                                                  
         BNO   MOX10                                                            
         GOTO1 DATAMGR,DMCB,=C'PURGE   ',WRKFILEN,WRKRINDX,IO2,AWKBUFF          
MOX10    DS    0H                                                               
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                             ERRORS                                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
HDRERROR OI    RECFLAG,HDRERR                                                   
         B     ERR10                                                            
                                                                                
BUYERROR OI    RECFLAG,BUYERR                                                   
ERR10    MVI   P1,0                                                             
         LA    R5,P2                                                            
         USING PRERRD,R5                                                        
         MVC   PRERRHD,=C'ERROR  LINE NUMBER'                                   
         EDIT  LINENUM,(8,PRERRNO)                                              
         MVC   P3(SBUYRLNQ),0(R2)                                               
         GOTO1 REPORT                                                           
         DROP  R5                                                               
         B     MO30                                                             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                   END-OF-BUY RECORD                                 *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
EOB      NTR1                                                                   
         USING SEBYD,R3                                                         
         TM    RECFLAG,BUYERR+HDRERR                                            
         BNZ   EXIT                                                             
         XC    SEBYODAT(SEBYSTRT-SEBYODAT),SEBYODAT                             
                                                                                
         MVC   SEBYLEN,=AL2(SEBYLENQ)                                           
         MVC   SEBYTYPE,=C'EBY*'   OBJECT TYPE                                  
                                                                                
         MVC   IO2L(2),=Y(SEBYLENQ+16)                                          
         BAS   RE,WRKR                                                          
                                                                                
         DROP  R3                                                               
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                   OPEN WORKER FILE                                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WRKROPEN NTR1                                                                   
         XC    SEQNUM,SEQNUM                                                    
         USING WLHDRD,R4                                                        
         LA    R4,IO2                                                           
         XC    0(255,R4),0(R4)     BUILD HEADER                                 
         MVC   WLSOFLAB,=C'*SOFSOF*'                                            
         MVC   WLUSRID,RCORIGID    COKEAT                                       
         MVC   WLSYSPRG(3),=C'CCX'                                              
         MVI   WLSUBPRG,C'1'       DESTINATION = FACADV1                        
         CLI   QOPT1,C'T'          OPTION TO GENERATE ON TST                    
         BNE   *+8                                                              
         MVI   WLSUBPRG,C'T'       DESTINATION = FACTST                         
         MVC   WLDAY,TDAY+2                                                     
         MVI   WLCLASS,C'T'        CLASS T FOR WRKF SCRIPTS                     
         MVI   WLTYPE,C'L'         TYPE L FOR EXEC AFTER 5:00                   
         CLI   QOPT5,C'A'                                                       
         BNE   *+8                                                              
         MVI   WLTYPE,C'A'         FORCE TO PROCESS NOW                         
         MVC   WLPSWD,SPACES                                                    
         OI    WLATTB,WLATOBJ                                                   
                                                                                
         LA    R3,IO2                                                           
         MVI   FIXED,C'Y'                                                       
         BAS   RE,WRKR                                                          
                                                                                
         XC    SEQNUM,SEQNUM                                                    
         XC    WRKRINDX,WRKRINDX                                                
         LA    R5,WRKRINDX                                                      
         USING UKRECD,R5                                                        
         MVC   UKUSRID,RCORIGID                                                 
         MVC   UKSYSPRG(3),=C'CCX'                                              
         MVC   UKFILENO,WLREPRNO   WORKER FILE NUMBER                           
         DROP  R5                                                               
                                                                                
         MVC   WRKFNO,WLREPRNO     WORKER FILE NUMBER                           
         MVI   FIXED,C'N'                                                       
                                                                                
         MVC   P(16),=C'WORKER FILE ID ='                                       
         EDIT  (B2,WLUSRID),(4,P+20)                                            
         MVI   P+24,C','                                                        
         MVC   P+25(4),WLFILEID                                                 
         GOTO1 HEXOUT,DMCB,WLDAY,P+29,1,=C'TOG'                                 
         MVC   P+31(1),WLCLASS                                                  
         MVI   P+32,C','                                                        
         EDIT  WLREPRNO,(5,P+33),0,ALIGN=LEFT                                   
         GOTO1 REPORT                                                           
         DROP  R4                                                               
                                                                                
         LA    R1,IO2                                                           
         XC    0(256,R1),0(R1)                                                  
         MVC   0(4,R1),=F'2101'                                                 
         MVC   4(6,R1),=C'SCRIPT'                                               
         MVC   10(8,R1),=C'SPCOKUPL'                                            
         MVI   18(R1),C'S'        INSERT MODE/USER SETS FINAL STATUS            
         MVC   30(5,R1),=C'00006'    HEADER LENGTH                              
         MVC   35(5,R1),=C'00100'    INPUT  DATA LENGTH                         
         MVC   40(5,R1),=C'00020'    OUTPUT DATA LENGTH                         
         MVI   45(R1),C'N'           DO NOT INSERT ERRORS AT FILE END           
         MVC   IO2L(2),=H'50'        46 + 4 BYTES FOR QSAM                      
         BAS   RE,WRKR                                                          
                                                                                
         LA    R1,IO2                                                           
         XC    0(256,R1),0(R1)                                                  
         MVC   10(20,R1),=CL20'SIGN-ON INFORMATION '                            
         MVC   30(6,R1),=C'COKEAT'                                              
         MVC   38(3,R1),=C'DDS'                                                 
                                                                                
         MVC   IO2L(2),=H'50'        LEN + 4 BYTES FOR QSAM                     
         BAS   RE,WRKR                                                          
                                                                                
         LA    R1,IO2                                                           
         XC    0(256,R1),0(R1)                                                  
         MVC   30(4,R1),=C'@#$%'                 MEDIA                          
         EDIT  WRKFNO,(10,34(R1)),0,ALIGN=LEFT   BUYER(WORKER FILE NO.)         
                                                                                
         MVC   IO2L(2),=H'47'                 43 + 4 BYTES FOR QSAM             
         BAS   RE,WRKR                                                          
         B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                   CLOSE WORKER FILE                                 *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WRKRCLSE NTR1                                                                   
         USING WLHDRD,R4                                                        
         LA    R4,IO2                                                           
         XC    0(255,R4),0(R4)     BUILD HEADER                                 
         MVC   WLSOFLAB,=C'*EOFEOF*'                                            
         MVI   FIXED,C'Y'                                                       
         XC    SEQNUM,SEQNUM                                                    
         BAS   RE,WRKR                                                          
                                                                                
         MVC   P(15),=C'LINES PROCESSED'                                        
         OC    STRTLN,STRTLN                                                    
         BNZ   *+12                                                             
         MVI   P+27,C'1'                                                        
         B     WRKRCL10                                                         
         EDIT  STRTLN,(8,P+20)                                                  
                                                                                
WRKRCL10 L     R5,LINENUM                                                       
         OC    LASTLN,LASTLN                                                    
         BZ    WRKRCL20                                                         
         C     R5,LASTLN                                                        
         BE    WRKRCL20                                                         
         BCTR  R5,0                                                             
                                                                                
WRKRCL20 EDIT  (R5),(8,P+30)                                                    
         GOTO1 REPORT                                                           
         MVC   STRTLN,LINENUM                                                   
         CLI   QOPT2,C'Y'          OPTION TO SET TO STATUS KEEP                 
         BNE   EXIT                                                             
         GOTO1 DATAMGR,DMCB,=C'KEEP    ',WRKFILEN,WRKRINDX,IO2,AWKBUFF          
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                   ADD LINE TO WORKER FILE                           *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WRKR     NTR1                                                                   
         OC    SEQNUM,SEQNUM                                                    
         BZ    WRKR10                                                           
         MVC   IO2(4),=F'2102'                                                  
         EDIT  SEQNUM,(6,IO2+4),0,FILL=0                                        
                                                                                
WRKR10   DS    0H                                                               
         LA    R3,IO2                                                           
         CLI   FIXED,C'Y'                                                       
         BE    *+8                                                              
         LA    R3,IO2L                                                          
         GOTO1 DATAMGR,DMCB,DMPRINT,WRKFILE,0,(R3),AWKBUFF                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         ST    R1,SEQNUM                                                        
EXIT     XIT1                                                                   
         EJECT                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
TIN      DCB   DDNAME=TIN,DSORG=PS,MACRF=GM,EODAD=MOX                           
                                                                                
         LTORG                                                                  
DEMTAB   DC    C'MET1 HH',C'META   '                                            
         DC    C'MET2 HH',C'METB   '                                            
         DC    C'DMA  HH',C'HOMES  '                                            
         DC    C'TOT  HH',C'HOMES  '                                            
         DC    X'FF'                                                            
                                                                                
LOWSTAB  DS    0H                     LOW POWER STATION TABLE                   
         DC    C'KM58',C'KMPH-L  '    MATCH (4), REPLACEMENT (8)                
         DC    X'FF'                                                            
                                                                                
ALPHABET DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ',X'FF'                              
         EJECT                                                                  
DMPRINT  DC    CL8'DMPRINT'                                                     
WRKFILE  DC    CL8'WRKF1  '                                                     
WRKFILEN DC    CL8'WRKFILE'                                                     
WRKFN    DC    C'SBUY'             WRKF=WRKF                                    
AWKBUFF  DC    A(WKBUFF)                                                        
SEQNUM   DS    F                   SEQUENCE NUMBER OF RECORDS                   
STARTLN  DS    F                   LINE TO START AT IN UPLOAD                   
LASTLN   DS    F                   LINE TO START AT IN UPLOAD                   
STRTLN   DS    F                   START LINE IN UPLOAD OF WORKER FILE          
LINENUM  DS    F                   LINE NUMBER IN WORKER FILE                   
NEXTALPH DS    F                   ADDRESS OF NEXT LETTER TO USE                
*                                                                               
RECFLAG  DS    X                   Y=NEXT H1 IS THE FIRST                       
HDRERR   EQU   X'80'               ERROR IN WORKER FILE HEADER                  
BUYERR   EQU   X'40'               ERROR IN WORKER FILE BUY                     
FNDLINE  EQU   X'20'               FOUND LINE AT WORKER FILE TO START           
PRGWRKR  EQU   X'10'               PURGE WORKER FILE                            
PROCSKD  EQU   X'08'               PROCESS SKD DATA AGAIN                       
PROCSKD2 EQU   X'04'               IN MIDDLE OF PROCESS SKD DATA AGAIN          
*                                                                               
WRKFNO   DS    XL2                 WORKER FILE NUMBER                           
TDAY     DS    CL3                 YYMMDD PWOS                                  
FIXED    DS    CL1                 FIXED LENGTH                                 
LASTREC  DS    C                                                                
PSKD     DS    PL2                                                              
PSKDTAB  DS    14PL2               TABLE OF PACKED SKEDULE VALUES               
WRKRINDX DS    CL42                                                             
SVBUY    DS    XL(SBUYLENQ+16)     BUY REC + 2(LEN) + 10(SEQ) + 4(LEN)          
SVSKD    DS    XL(SSKDLENQ+2)      SKD REC + 2(LENGTH)                          
IO       DS    XL256               INPUT IO AREA                                
IO2L     DS    F                                                                
IO2      DS    XL400               OUTPUT IO AREA                               
WKBUFF   DS    14336C                                                           
*                                                                               
PRERRD   DSECT                                                                  
PRERRHD  DS    CL18                                                             
         DS    C                                                                
PRERRNO  DS    CL8                                                              
         DS    CL3                                                              
PRERRMSG DS    CL25                                                             
         DS    CL5                                                              
PRERRFLD DS    CL18                                                             
*                                                                               
         EJECT                                                                  
                                                                                
         PRINT OFF                                                              
       ++INCLUDE SPTUPLOADD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE DMWRKFL                                                        
       ++INCLUDE DMWRKFK                                                        
       ++INCLUDE CTGENFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012SPREPMO02 08/28/02'                                      
         END                                                                    
