*          DATA SET STRECONOS  AT LEVEL 005 AS OF 05/01/02                      
*PHASE STRECONT                                                                 
*INCLUDE SORTER                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
         TITLE 'SPOTPAK - STATION FILE RECONSTRUCT PROGRAM'                     
         PRINT NOGEN                                                            
STRECON  CSECT                                                                  
         NBASE 0,**RECON,=V(REGSAVE),R6                                         
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                  INITIALIZE SORT                              
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
         LA    R7,RCVTAPE          R7=A(INPUT RECOVERY TAPE DTF)                
*                                                                               
*                                                                               
STR2     OPEN  (R7)                                                             
         AP    INPFILE,=P'1'       UPDATE INPUT FILE COUNT                      
         XC    RCOUNT,RCOUNT                                                    
         LA    R4,RECVREC                                                       
         USING DRCVR,R4                                                         
         EJECT                                                                  
*                                  READ RECORDS & PASS TO SORTER                
*                                                                               
STR4     GET   (R7),(R4)                                                        
         CLI   RFILTY,STFILE                                                    
         BNE   STR4                                                             
         CLI   RRECTY,ADD          IGNORE FUNNIES                               
         BH    STR4                                                             
         SR    R1,R1               UPDATE INPUT ACCUMS                          
         IC    R1,RRECTY                                                        
         BCTR  R1,0                                                             
         MH    R1,=H'20'                                                        
         LA    R1,RCVACCS(R1)                                                   
         AP    0(4,R1),=P'1'                                                    
         CLI   RRECTY,COPY         IGNORE COPIES                                
         BE    STR4                                                             
         L     R1,RCOUNT           UPDATE MY COUNT                              
         AH    R1,=H'1'                                                         
         ST    R1,RCOUNT                                                        
         MVC   RSIN,RCOUNT         AND MOVE TO RECORD                           
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R4)                                     
         B     STR4                                                             
*                                                                               
STR6     CLOSE (R7)                                                             
*                                  WAS THAT THE ONLY RECOVERY TAPE              
STR8     GOTO1 =V(LOGIO),DMCB,1,(40,MESSG)                                      
         MVC   WORK,SPACES                                                      
         GOTO1 (RF),(R1),0,(3,WORK)                                             
         CLC   WORK(3),=C'YES'                                                  
         BE    STR2                                                             
         CLC   WORK(2),=C'NO'                                                   
         BNE   STR8                                                             
         LA    R7,INTAPE           R7=A(INPUT DUMP TAPE DTF)                    
         LA    R8,OUTAPE           R8=A(OUTPUT DUMP TAPE DTF)                   
         OPEN  (R7),(R8)                                                        
         XC    RECVREC(4),RECVREC  SET FT INDIC                                 
         XC    RECORD(4),RECORD                                                 
         BAS   RE,GETREC           AND GET A RECOVERY RECORD                    
         EJECT                                                                  
*                                                                               
STR10    GET   (R7),DUMPREC                                                     
         AP    INPACCS,=P'1'                                                    
*                                                                               
STR12    CLI   EOFSORT,C'Y'                                                     
         BE    STR20                                                            
         CLI   RCVTYPE,ADD                                                      
         BE    STR16                                                            
         CLI   RCVTYPE,CHANGE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  CHANGE                                       
         CLC   DUMPREC+4(17),RECVREC+4                                          
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
STR16    CLC   DUMPREC+4(17),RECVREC+4                                          
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
STR22    CLOSE (R7),(R8)                                                        
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
STR26    EOJ                                                                    
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
         BNZ   *+12                                                             
         MVI   EOFSORT,C'X'                                                     
         B     XIT                                                              
         LH    R3,0(R4)            MOVE RECORD TO MY IO AREA                    
         SH    R3,=H'28'                                                        
         MOVE  (RECORD+4,(R3)),28(R4)                                           
         LA    R3,4(R3)                                                         
         STH   R3,RECORD           SET LENGTH                                   
         SR    R1,R1                                                            
         IC    R1,5(R4)            UPDATE SORT ACCUMS                           
         BCTR  R1,0                                                             
         MH    R1,=H'20'                                                        
         LA    R1,SRTACCS(R1)                                                   
         AP    0(4,R1),=P'1'                                                    
         CLC   RECORD+4(17),RECVREC+4   DUPLICATE KEYS                          
         BE    GETREC2                  YES - READ UNTIL NOT                    
         MVC   RECTYPE,5(R4)       SET ACTION                                   
         OC    RECVREC(4),RECVREC  FTI SET                                      
         BZ    GETREC2             YES - GO GET ANOTHER ONE                     
         B     XIT                                                              
*                                  PUT TO OUTPUT DUMP TAPE                      
PUTREC   NTR1                                                                   
         PUT   (R8),(R4)                                                        
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
         GOTO1 =V(HEXOUT),DMCB,DUMPREC+4,P+18,17,=C'TOG'                        
         GOTO1 =V(PRINTER)                                                      
         MVC   P(18),=C'RECOVERY RECORD - '                                     
         GOTO1 =V(HEXOUT),DMCB,RECVREC+4,P+18,17,=C'TOG'                        
         GOTO1 =V(PRINTER)                                                      
         BASR  RE,RF                                                            
         AP    ERRACCS,=P'1'                                                    
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              RECOVERY TAPE DTF                                                
*                                                                               
RCVTAPE  DTFMT RECFORM=VARBLK,BLKSIZE=8200,DEVADDR=SYS004,WORKA=YES,   *        
               FILABL=STD,IOAREA1=BLKA,EOFADDR=STR6,REWIND=UNLOAD               
*                                                                               
*              INPUT DUMP TAPE DTF                                              
*                                                                               
INTAPE   DTFMT RECFORM=VARBLK,BLKSIZE=8200,DEVADDR=SYS005,             *        
               WORKA=YES,FILABL=STD,IOAREA1=BLKA,EOFADDR=STR22                  
*                                                                               
*              OUTPUT DUMP TAPE DTF                                             
*                                                                               
OUTAPE   DTFMT RECFORM=VARBLK,BLKSIZE=8200,DEVADDR=SYS006,             *        
               WORKA=YES,FILABL=STD,IOAREA1=BLKB,TYPEFLE=OUTPUT                 
         EJECT                                                                  
*              LITERALS ETC                                                     
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
DMCB     DS    6F                                                               
RCOUNT   DS    F                                                                
WORK     DS    CL20                                                             
SORTCARD DC    C'SORT FIELDS=(29,17,A,23,3,A,9,4,A),FORMAT=BI,WORK=1 '          
RECCARD  DC    C'RECORD TYPE=V,LENGTH=(200,,,,) '                               
EOFSORT  DC    C'N'                                                             
ERRNUM   DS    C                                                                
RCVTYPE  DS    C                                                                
RECTYPE  DS    C                                                                
ERRLIST  DS    0CL40                                                            
         DC    CL40'MISSING RECORD FOR CHANGE ON DUMP TAPE'                     
         DC    CL40'DUPLICATE RECORD FOR ADD ON DUMP TAPE'                      
MESSG    DC    CL40'ANY MORE RECOVERY TAPES TO BE MERGED ?'                     
ACCS     DS    0CL20                                                            
INPFILE  DC    PL4'0',CL16'INPUT FILES'                                         
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
         DS    0F                                                               
DUMPREC  DS    200C                                                             
RECORD   DS    200C                                                             
RECVREC  DS    2100C                                                            
BLKA     DS    8200C                                                            
BLKB     DS    8200C                                                            
         EJECT                                                                  
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
STFILE   EQU   X'22'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005STRECONOS 05/01/02'                                      
         END                                                                    
