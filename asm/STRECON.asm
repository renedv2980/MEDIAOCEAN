*          DATA SET STRECON    AT LEVEL 011 AS OF 06/07/11                      
*PHASE STRECONA                                                                 
*INCLUDE SORTER                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE REGSAVE                                                                
         TITLE 'SPOTPAK - STATION FILE RECONSTRUCT PROGRAM'                     
         PRINT NOGEN                                                            
STRECON  CSECT                                                                  
         NBASE 0,**RECON,=V(REGSAVE),R6                                         
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
*                                                                               
         OPEN  (RECVIN,INPUT)      OPEN RECOVERY TAPE                           
         LA    R4,RECVREC                                                       
         USING DRCVR,R4                                                         
         EJECT                                                                  
*                                  READ RECORDS & PASS TO SORTER                
*                                                                               
STR4     GET   RECVIN,(R4)         GET A RECORD                                 
*                                                                               
         CLI   RRECTY,X'81'        IGNORE POINTER COPY/CHANGES                  
         BE    STR4                                                             
         CLI   RRECTY,X'82'                                                     
         BE    STR4                                                             
         CLI   RSIN,X'FF'          IGNORE DELETED RECORDS                       
         BE    STR4                                                             
         CLI   RSIN,X'FE'          OFFLINE COPY/CHANGE PAIR                     
         BNE   *+10                                                             
         MVC   RSIN,=F'1'                                                       
         CLI   RSIN,X'FD'          OFFLINE CHANGES ONLY                         
         BNE   *+10                                                             
         MVC   RSIN,=F'0'                                                       
*                                                                               
         CLI   RFILTY,QSTFILE      STATION FILE?                                
         BNE   STR4                NO SKIP IT                                   
         CLI   RRECTY,ADD          IGNORE FUNNIES                               
         BH    STR4                NOT 1,2 OR 3 SKIP IT                         
*                                                                               
         LH    R1,RECVREC          POINT R1 TO END OF RECORD                    
         LA    R1,RECVREC(R1)                                                   
         TM    RTIME,X'40'         TEST IF THERE IS A TRAILER                   
         BZ    STR5                NO                                           
         NI    RTIME,255-X'40'     YES-REMOVE TRAILER                           
         BCTR  R1,0                                                             
         SR    RF,RF                                                            
         IC    RF,0(R1)            RF=L'TRAILER IN LAST BYTE                    
         SR    R1,RF                                                            
         LA    R1,1(R1)            R1=A(END OF RECORD WITHOUT TRAILER)          
         LA    RF,RECVREC                                                       
         LR    RE,R1                                                            
         SR    RE,RF                                                            
         STH   RE,RECVREC          SET NEW RECORD LENGTH                        
*                                                                               
STR5     MVI   0(R1),0             SET ZERO AT END OF RECORD                    
*                                                                               
         SR    R1,R1               UPDATE INPUT ACCUMS                          
         IC    R1,RRECTY           TYPE OF RECORD                               
         BCTR  R1,0                                                             
         MH    R1,=H'20'           MULTIPLY BY 20                               
         LA    R1,RCVACCS(R1)      GET ADDRESS OF ACCUMLATORS                   
         AP    0(4,R1),=P'1'       ADD ONE TO ACCUMULATOR                       
*                                                                               
         CLI   RRECTY,COPY         IGNORE COPIES                                
         BE    STR4                                                             
         L     R1,RCOUNT           UPDATE MY COUNT                              
         AH    R1,=H'1'                                                         
         ST    R1,RCOUNT                                                        
         MVC   RSIN,RCOUNT         AND MOVE TO RECORD                           
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R4)                                     
         B     STR4                                                             
*                                                                               
STR6     CLOSE (RECVIN,)           CLOSE RECOVERY TAPE                          
         OPEN  (DUMPIN,INPUT)      GET DUMP TAPE                                
         OPEN  (DUMPOUT,OUTPUT)    PREPARE NEW DUMP                             
         XC    RECVREC(4),RECVREC  SET FT INDIC                                 
         XC    RECORD(4),RECORD                                                 
         BAS   RE,GETREC           AND GET A RECOVERY RECORD                    
         LH    R1,RECORD           IS IT THE FIRST TIME?                        
         LTR   R1,R1                                                            
         BNZ   STR10               NOPE, DON'T WORRY ABOUT EOFS                 
         CLI   EOFSORT,C'X'        IS THERE AN EOF THE FIRST TIME?              
         BNE   STR10               NO, CONTINUE                                 
         MVI   EOFSORT,C'Y'        YES, JUST COPY OLD RECORDS                   
         EJECT                                                                  
*                                                                               
STR10    GET   DUMPIN,DUMPREC                                                   
         AP    INPACCS,=P'1'                                                    
*                                                                               
STR12    CLI   EOFSORT,C'Y'        COPY OLD RECORD?                             
         BE    STR20                                                            
         CLI   RCVTYPE,ADD         A NEW RECORD?                                
         BE    STR16                                                            
         CLI   RCVTYPE,CHANGE      A CHANGE ON A RECORD?                        
         BE    *+6                                                              
         DC    H'0'                ERROR!  DUMP                                 
*                                  CHANGE                                       
         CLC   DUMPREC+4(15),RECVREC+4                                          
         BL    STR20               NOT THERE YET                                
         BE    STR14               GOT  IT                                      
         MVI   ERRNUM,1            OTHERWISE IS ERROR                           
         BAS   RE,BADREC                                                        
         BAS   RE,GETREC                                                        
         B     STR12                                                            
*                                  PUT RECOVERY REC TO OUTPUT TAPE              
STR14    LA    R4,RECVREC                                                       
         BAS   RE,PUTREC                                                        
         BAS   RE,GETREC                                                        
         B     STR10                                                            
*                                  ADD                                          
STR16    CLC   DUMPREC+4(15),RECVREC+4                                          
         BL    STR20               NOT THERE YET                                
         BH    STR18               GOT IT                                       
         MVI   ERRNUM,2            OTHERWISE IS ERROR                           
         BAS   RE,BADREC                                                        
         BAS   RE,GETREC                                                        
         B     STR12                                                            
*                                  PUT RECOVERY REC TO OUTPUT TAPE              
STR18    LA    R4,RECVREC                                                       
         BAS   RE,PUTREC                                                        
         AP    CHAACCS,=P'1'                                                    
         BAS   RE,GETREC                                                        
         B     STR12               CHECK FOR MULTIPLE ADD                       
*                                  PUT DUMP RECORD TO OUTPUT TAPE               
STR20    LA    R4,DUMPREC                                                       
         BAS   RE,PUTREC                                                        
         B     STR10                                                            
         EJECT                                                                  
*                                  OUTPUT RECORD COUNTS TO CONSOLE              
*                                  AND PRINTER                                  
STR22    CLOSE (DUMPIN,)                                                        
         CLOSE (DUMPOUT,)                                                       
         MVC   TITLE(25),=C'RECONSTRUCT RECORD COUNTS'                          
         ZAP   LINE,=P'99'                                                      
         ZAP   PAGE,=P'1'                                                       
         LA    R4,ACCS                                                          
*                                                                               
STR24    CLI   0(R4),X'FF'                                                      
         BE    STR26                                                            
         MVC   P(16),4(R4)                                                      
         UNPK  P+18(6),0(4,R4)                                                  
         OI    P+23,X'F0'                                                       
         GOTO1 =V(LOGIO),DMCB,1,(24,P)                                          
         GOTO1 =V(PRINTER)                                                      
         LA    R4,L'ACCS(R4)                                                    
         B     STR24                                                            
*                                                                               
STR26    XBASE                                                                  
         EJECT                                                                  
*              GET A RECORD FROM SORTED RECOVERY TAPE                           
*                                                                               
GETREC   NTR1                                                                   
*                                                                               
GETREC2  LH    R1,RECORD                                                        
         LTR   R1,R1               FTI SET                                      
         BZ    GETREC4                                                          
         MOVE  (RECVREC,(R1)),RECORD                                            
         MVC   RCVTYPE,RECTYPE                                                  
         CLI   EOFSORT,C'X'        IF DUMMY EOF WAS SET                         
         BNE   *+12                                                             
         MVI   EOFSORT,C'Y'        SET REAL EOF AND RETURN                      
         B     XIT                                                              
*                                                                               
GETREC4  GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         L     R4,DMCB+4                                                        
         LTR   R4,R4               EOF ?                                        
         BNZ   NOTX                                                             
         MVI   EOFSORT,C'X'                                                     
         B     XIT                                                              
NOTX     LH    R3,0(R4)            MOVE RECORD TO MY IO AREA                    
         SH    R3,=H'28'           L'RECORD - (RECVHDR + LENGTH)                
         MOVE  (RECORD+4,(R3)),28(R4)                                           
         LA    R3,4(R3)                                                         
         STH   R3,RECORD           SET LENGTH                                   
         SR    R1,R1                                                            
         IC    R1,5(R4)            UPDATE SORT ACCUMS (RRECTY)                  
         BCTR  R1,0                                                             
         MH    R1,=H'20'                                                        
         LA    R1,SRTACCS(R1)                                                   
         AP    0(4,R1),=P'1'                                                    
         CLC   RECORD+4(15),RECVREC+4   DUPLICATE KEYS                          
         BNE   NOTDUP                   YES - READ UNTIL NOT                    
*********MVC   P,=CL132'DUPLICATE!'                                             
*********GOTO1 =V(PRINTER)                                                      
         B     GETREC2                                                          
NOTDUP   MVC   RECTYPE,5(R4)       SET ACTION                                   
         OC    RECVREC(4),RECVREC  FTI SET                                      
         BZ    GETREC2             YES - GO GET ANOTHER ONE                     
         B     XIT                                                              
*                                  PUT TO OUTPUT DUMP TAPE                      
PUTREC   NTR1                                                                   
         PUT   DUMPOUT,(4)                                                      
         AP    OUTACCS,=P'1'       UPDATE OUTPUT ACCUMS                         
         B     XIT                                                              
         EJECT                                                                  
*              HANDLE BAD RECORDS                                               
*                                                                               
BADREC   NTR1                                                                   
         SR    R1,R1                                                            
         IC    R1,ERRNUM                                                        
         BCTR  R1,0                                                             
         MH    R1,=H'40'                                                        
         LA    R1,ERRLIST(R1)                                                   
         MVC   P(40),0(R1)                                                      
         GOTO1 =V(PRINTER)                                                      
         MVC   P(18),=C'INPUT RECORD    - '                                     
         GOTO1 =V(HEXOUT),DMCB,DUMPREC+4,P+18,15,=C'TOG'                        
         GOTO1 =V(PRINTER)                                                      
         MVC   P(18),=C'RECOVERY RECORD - '                                     
         GOTO1 =V(HEXOUT),DMCB,RECVREC+4,P+18,15,=C'TOG'                        
         GOTO1 =V(PRINTER)                                                      
         BASR  RE,RF                                                            
         AP    ERRACCS,=P'1'                                                    
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              RECOVERY TAPE DCB                                                
*                                                                               
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,MACRF=GM,EODAD=STR6              
*                                                                               
*              INPUT DUMP TAPE DCB                                              
*                                                                               
DUMPIN   DCB   DDNAME=DUMPIN,DSORG=PS,MACRF=GM,EODAD=STR22                      
*                                                                               
*              OUTPUT DUMP TAPE DCB                                             
*                                                                               
DUMPOUT  DCB   DDNAME=DUMPOUT,DSORG=PS,RECFM=VB,LRECL=8200,            *        
               BLKSIZE=27648,MACRF=PM                                           
         EJECT                                                                  
*              LITERALS ETC                                                     
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
RCOUNT   DC    F'0'                                                             
WORK     DS    CL20                                                             
SORTCARD DC    C'SORT FIELDS=(29,15,A,9,4,A),FORMAT=BI,WORK=1 '                 
RECCARD  DC    C'RECORD TYPE=V,LENGTH=(1024,,,,) '                              
EOFSORT  DC    C'N'                                                             
CHKSIN   DC    C'N'                                                             
ERRNUM   DS    C                                                                
RCVTYPE  DS    C                                                                
RECTYPE  DS    C                                                                
ERRLIST  DS    0CL40                                                            
         DC    CL40'MISSING RECORD FOR CHANGE ON DUMP TAPE'                     
         DC    CL40'DUPLICATE RECORD FOR ADD ON DUMP TAPE'                      
ACCS     DS    0CL20                                                            
RCVACCS  DC    PL4'0',CL16'RECOVERY COPIES'                                     
         DC    PL4'0',CL16'RECOVERY CHANGES'                                    
         DC    PL4'0',CL16'RECOVERY ADDS'                                       
SRTACCS  DC    PL4'0',CL16'SORTED COPIES'                                       
         DC    PL4'0',CL16'SORTED CHANGES'                                      
         DC    PL4'0',CL16'SORTED ADDS'                                         
INPACCS  DC    PL4'0',CL16'INPUT RECORDS'                                       
ERRACCS  DC    PL4'0',CL16'INPUT ERRORS'                                        
CHAACCS  DC    PL4'0',CL16'NET CHANGES'                                         
OUTACCS  DC    PL4'0',CL16'OUTPUT RECORDS'                                      
         DC    X'FF'                                                            
*                                                                               
         DS    0F                                                               
DUMPREC  DS    1024C                                                            
RECORD   DS    1024C                                                            
RECVREC  DS    4100C                                                            
         EJECT                                                                  
* DUMP LIST                                                                     
DUMPLIST DS    0F                                                               
         DC    A(STRECON,65000)                                                 
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
* DDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
* DMRCVRHDR                                                                     
DRCVR    DSECT                                                                  
         DS    F                                                                
       ++INCLUDE DMRCVRHDR                                                      
         EJECT                                                                  
ADD      EQU   3                                                                
COPY     EQU   1                                                                
CHANGE   EQU   2                                                                
QSTFILE  EQU   X'22'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011STRECON   06/07/11'                                      
         END                                                                    
