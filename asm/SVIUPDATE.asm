*          DATA SET SVIUPDATE  AT LEVEL 030 AS OF 05/01/02                      
*PHASE SVIUPD,*                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE LOGIO                                                                  
SVISORT  DDSRT1 SVIUPD,40                                                       
         EJECT                                                                  
         PRINT NOGEN                                                            
SVIUPD   CSECT                                                                  
         NMOD1 0,SVIUPD                                                         
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         ST    R1,SAVER1                                                        
         LM    R2,R4,0(R1)                                                      
         LA    R2,BRANCH(R2)                                                    
         BR    R2                                                               
*                                                                               
BRANCH   B     INITIAL                                                          
         B     INPUT                                                            
         B     OUTPUT                                                           
         B     FINAL                                                            
*                                                                               
EXIT     L     R1,SAVER1                                                        
         XMOD1 1                                                                
         EJECT                                                                  
INITIAL  MVC   0(37,R3),=C'SORT FIELDS=(1,23,A),FORMAT=BI,WORK=1'               
         MVC   0(23,R4),=C'RECORD TYPE=F,LENGTH=80'                             
INITIAL2 MVI   FIRST,1                                                          
         MVI   UPDFRST,1                                                        
         B     EXIT                                                             
         EJECT                                                                  
INPUT    ST    R1,SAVER1                                                        
         CLI   FIRST,1                                                          
         BE    INSET                                                            
ERRBYP   CLI   TAPESW,1                                                         
         BNE   INPUT1                                                           
         GET   IN1,CARD                                                         
         B     INPUT2                                                           
INPUT1   GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   CARD(2),=C'/*'      END OF CARDS                                 
         BE    INPUTEND                                                         
INPUT2   MVC   TEMP(4),=C'0000'         EDIT MARKET                             
         MVZ   TEMP(4),CMKT                                                     
         CLC   TEMP(4),=C'0000'                                                 
         BNE   MKTERR                                                           
         CLC   CDAY,=C'M-F'                                                     
         BE    *+10                                                             
         CLC   CDAY,=C'SUN'                                                     
         BE    *+10                                                             
         CLC   CDAY,=C'SAT'                                                     
         BE    *+8                                                              
         B     DAYERR                                                           
         MVI   CDAY-1,X'15'                                                     
         CLC   CDAY(3),=C'SUN'                                                  
         BNE   *+8                                                              
         MVI   CDAY-1,X'77'                                                     
         CLC   CDAY(3),=C'SAT'                                                  
         BNE   *+8                                                              
         MVI   CDAY-1,X'66'                                                     
         CLC   CSTQH,CENDQH                                                     
         BH    QHERR                                                            
         MVC   HALF,CSTQH                                                       
         BAS   R9,QHEDIT                                                        
         MVC   HALF,CENDQH                                                      
         BAS   R9,QHEDIT                                                        
         CLC   CAUD,=C'20'                                                      
         BH    AUDERR                                                           
         CLI   CACT,C'D'                                                        
         BE    INPUTX                                                           
         CLI   CACT,C'A'                                                        
         BE    *+8                                                              
         CLI   CACT,C'C'                                                        
         BE    *+8                                                              
         B     ACTERR                                                           
         LA    R1,CMONS+1                                                       
         LA    R3,12                                                            
INPUT3   BAS   R9,MONEDIT                                                       
         LA    R1,4(R1)                                                         
         BCT   R3,INPUT3                                                        
         B     INPUTX                                                           
         SPACE 2                                                                
INSET    MVC   TITLE(23),=C'SVI UPDATE INPUT ERRORS'                            
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   CARD(2),=C'/*'      NO CARDS                                     
         BE    INPUTEND                                                         
         MVI   TAPESW,0                                                         
         MVI   FIRST,0                                                          
         CLC   CARD(10),=C'INPUT=TAPE'                                          
         BNE   INPUT2                                                           
         OPEN  IN1                                                              
         MVI   TAPESW,1                                                         
         GET   IN1,CARD                                                         
         B     INPUT2                                                           
         EJECT                                                                  
QHEDIT   MVC   TEMP(2),=C'00'                                                   
         MVZ   TEMP(2),HALF                                                     
         CLC   TEMP(2),=C'00'                                                   
         BNE   QHERR                                                            
         CLC   HALF,=C'96'                                                      
         BH    QHERR                                                            
         BR    R9                                                               
         SPACE 2                                                                
MONEDIT  MVC   TEMP(3),=C'000'                                                  
         MVZ   TEMP(3),0(R1)                                                    
         CLC   TEMP(3),=C'000'                                                  
         BNE   MONERR                                                           
         CLC   0(3,R1),=C'001'                                                  
         BL    MONERR                                                           
         CLC   0(3,R1),=C'255'                                                  
         BH    MONERR                                                           
         BR    R9                                                               
         EJECT                                                                  
MEDERR   MVC   P+90(19),=C'** INVALID MEDIA **'                                 
         B     INERR                                                            
         SPACE 2                                                                
RATERR   MVC   P+90(28),=C'** INVALID RATING SERVICE **'                        
         B     INERR                                                            
         SPACE 2                                                                
SCRERR   MVC   P+90(25),=C'** INVALID SOURCE CODE **'                           
         B     INERR                                                            
         SPACE 2                                                                
MKTERR   MVC   P+90(20),=C'** INVALID MARKET **'                                
         B     INERR                                                            
         SPACE 2                                                                
DAYERR   MVC   P+90(29),=C'** DAY MUST BE M-F,SAT,SUN **'                       
         B     INERR                                                            
         SPACE 2                                                                
QHERR    MVC   P+90(28),=C'** INVALID QH OR AUD TYPE **'                        
         B     INERR                                                            
         SPACE 2                                                                
AUDERR   MVC   P+90(28),=C'** AND TYPE MUST BE 01-10 **'                        
         B     INERR                                                            
         SPACE 2                                                                
ACTERR   MVC   P+90(26),=C'** ACTION MUST BE A,C,D **'                          
         B     INERR                                                            
         SPACE 2                                                                
MONERR   MVC   P+90(32),=C'** MONTH SVI MUST BE 001-255 **'                     
         B     INERR                                                            
         SPACE 2                                                                
INERR    MVC   P+5(80),CARD                                                     
         GOTO1 =V(PRINTER)                                                      
         B     ERRBYP                                                           
         EJECT                                                                  
INPUTX   L     R1,SAVER1                                                        
         LA    R2,CARD                                                          
         ST    R2,4(R1)                                                         
         B     EXIT                                                             
         SPACE 2                                                                
INPUTEND L     R1,SAVER1                                                        
         MVC   0(4,R1),=F'8'                                                    
         XC    RECORD(255),RECORD                                               
         MVI   UPDFRST,1                                                        
         XC    PREVCARD,PREVCARD                                                
         B     EXIT                                                             
         EJECT                                                                  
OUTPUT   ST    R1,SAVER1                                                        
         MVC   TITLE(26),=C'SVI UPDATE - INPUT ELISTING'                        
         MVC   CARD(80),0(R3)                                                   
         CLC   CARD(23),PREVCARD                                                
         BE    DUPERR                                                           
         CLC   CARD(18),PREVCARD                                                
         BNE   PUTOUT                                                           
OUTPUT2  MVC   PREVCARD,CARD                                                    
         LA    R4,ELEM             BUILD ADJUSTMENT RECORD                      
         USING SRKEYD,R4                                                        
         MVI   SRCODE,C'S'                                                      
         MVC   SRMED,CMED                                                       
         MVC   SRSRC,CRAT                                                       
         MVC   SRSCODE,CSRC                                                     
         PACK  DUB,CMKT                                                         
         CVB   R5,DUB                                                           
         STH   R5,HALF                                                          
         MVC   SRHIMAR,HALF                                                     
         CLI   CACT,C'D'                                                        
         BE    PUTOUT                                                           
         MVC   SRHIDAY,CDAY-1                                                   
         PACK  DUB,CSTQH                                                        
         CVB   R5,DUB                                                           
         STC   R5,SRHIQH                                                        
         PACK  DUB,CENDQH                                                       
         CVB   R5,DUB                                                           
         BCTR  R5,0                                                             
         STC   R5,SRLOWQH                                                       
         MVC   SRLEN,=H'26'                                                     
         DROP  R4                                                               
         LA    R4,11(R4)                                                        
         USING SVELEM,R4                                                        
         MVI   SVCODE,2                                                         
         MVI   SVLEN,X'0F'                                                      
         PACK  DUB,CAUD                                                         
         CVB   R5,DUB                                                           
         STC   R5,SVTYP                                                         
         LA    R7,SVJAN                                                         
         LA    R8,CMONS                                                         
         LA    R9,12                                                            
OUTPUT3  PACK  DUB,1(3,R8)                                                      
         CVB   R5,DUB                                                           
         STC   R5,0(R7)                                                         
         LA    R7,1(R7)                                                         
         LA    R8,4(R8)                                                         
         BCT   R9,OUTPUT3                                                       
         OC    RECORD(11),RECORD                                                
         BNZ   OUTPUT4                                                          
         MVC   RECORD(26),ELEM                                                  
         XC    ELEM,ELEM                                                        
         B     OUTPUT5                                                          
OUTPUT4  MVC   HALF,RECORD+9                                                    
         LA    R7,RECORD                                                        
         AH    R7,HALF                                                          
         MVC   0(15,R7),0(R4)                                                   
         LH    R6,HALF                                                          
         LA    R6,15(R6)                                                        
         STH   R6,HALF                                                          
         MVC   RECORD+9(2),HALF                                                 
OUTPUT5  DS    0H                                                               
         B     EXIT                                                             
         L     R1,SAVER1                                                        
         MVC   P+5(80),CARD                                                     
         GOTO1 =V(PRINTER)                                                      
         B     EXIT                                                             
         EJECT                                                                  
DUPERR   MVC   P+5(80),CARD                                                     
         MVC   P+90(24),=C'DUPLICATE INPUT BYPASSED'                            
         GOTO1 =V(PRINTER)                                                      
         L     R1,SAVER1                                                        
         B     EXIT                                                             
         EJECT                                                                  
PUTOUT   CLI   UPDFRST,1                                                        
         BNE   PUTOUT1                                                          
         OPEN  IN,OUT                                                           
         GET   IN,INREC-4                                                       
         L     RE,OLDCNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,OLDCNT                                                        
         MVI   UPDFRST,0                                                        
         XC    RECORD(255),RECORD                                               
         B     OUTPUT2                                                          
         SPACE 2                                                                
PUTOUT1  CLC   INREC(11),RECORD                                                 
         BE    BYPIN                                                            
         BL    COPYIN                                                           
         MVI   OUTSW,C'R'          ADD NEW RECORD                               
         OC    RECORD(11),RECORD                                                
         BZ    OUTPUT2                                                          
         XC    RECORD-4(4),RECORD-4                                             
         MVC   HALF(2),RECORD+9                                                 
         LH    RE,HALF                                                          
         LA    RE,4(RE)                                                         
         STH   RE,HALF                                                          
         MVC   RECORD-4(2),HALF                                                 
         PUT   OUT,RECORD-4                                                     
         L     RE,OUTCNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,OUTCNT                                                        
         XC    RECORD(255),RECORD                                               
         B     OUTPUT2                                                          
         SPACE 2                                                                
BYPIN    GET   IN,INREC-4          BYPASS INPUT                                 
         L     RE,OLDCNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,OLDCNT                                                        
         B     PUTOUT1                                                          
         SPACE 2                                                                
COPYIN   PUT   OUT,INREC-4                                                      
         L     RE,OUTCNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,OUTCNT                                                        
         B     BYPIN                                                            
         EJECT                                                                  
FINAL    OC    RECORD(10),RECORD                                                
         BZ    FINAL2                                                           
         XC    RECORD-4(4),RECORD-4                                             
         MVC   HALF,RECORD+9                                                    
         LH    RE,HALF                                                          
         LA    RE,4(RE)                                                         
         STH   RE,HALF                                                          
         MVC   RECORD-4(2),HALF                                                 
         PUT   OUT,RECORD-4                                                     
         XC    RECORD(255),RECORD                                               
FINAL2   DS    0H                                                               
         CLI   INREC,X'FF'                                                      
         BE    FINALX                                                           
         PUT   OUT,INREC-4                                                      
         L     RE,OUTCNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,OUTCNT                                                        
         GET   IN,INREC-4                                                       
         L     RE,OLDCNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,OLDCNT                                                        
         B     FINAL                                                            
         SPACE 2                                                                
FINALX   DS    0H                                                               
         MVC   P(16),=C'EXISTING RECORDS'                                       
         EDIT  OLDCNT,(6,P+20)                                                  
         GOTO1 =V(PRINTER)                                                      
         MVC   P(16),=C'  OUTPUT RECORDS'                                       
         EDIT  OUTCNT,(6,P+20)                                                  
         GOTO1 =V(PRINTER)                                                      
         CLOSE IN,OUT                                                           
         B     EXIT                                                             
         EJECT                                                                  
PUTOUTX  B     EXIT                                                             
INEND    MVI   INREC,X'FF'                                                      
         MVC   INREC+1(256),INREC                                               
         OC    RECORD(20),RECORD                                                
         BZ    FINALX                                                           
         B     PUTOUT1                                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
OUTCNT   DC    F'0'                                                             
OLDCNT   DC    F'0'                                                             
DMCB     DS    6F                                                               
DUB      DS    D                                                                
WORK     DS    CL17                                                             
HALF     DS    H                                                                
PREVCARD DS    CL23                                                             
UPDFRST  DC    X'01'                                                            
TAPESW   DC    X'00'                                                            
OUTSW    DS    C                                                                
TEMP     DS    CL4                                                              
FIRST    DC    X'01'                                                            
SAVER1   DS    F                                                                
CARD     DS    0H                                                               
CMED     DS    CL1                 MEDIA                                        
CRAT     DS    CL1                 SERVICE                                      
CSRC     DS    CL1                 SOURCE CODE                                  
         DS    CL1                                                              
CMKT     DS    CL4                 MARKET                                       
         DS    CL1                                                              
CDAY     DS    CL3                 DAY - M-F,SAT,SUN                            
         DS    CL1                                                              
CSTQH    DS    CL2                 START QTR HOUR                               
         DS    CL1                                                              
CENDQH   DS    CL2                 END QTR HOUR                                 
         DS    CL1                                                              
CAUD     DS    CL2                 AUDIENCE TYPE                                
         DS    CL1                                                              
CACT     DS    CL1                 ACTION                                       
         DS    CL1                                                              
CMONS    DS    CL48                SVI FACTORS                                  
         DS    CL8                                                              
ELEM     DS    CL40                                                             
         DS    CL4                                                              
RECORD   DS    CL300                                                            
         DS    CL4                                                              
INREC    DS    CL300                                                            
IN       DTFMT DEVADDR=SYS004,BLKSIZE=8000,RECFORM=VARBLK,             X        
               TYPEFLE=INPUT,IOAREA1=INA,WORKA=YES,FILABL=STD,         X        
               EOFADDR=INEND,REWIND=UNLOAD                                      
INA      DS    8000C                                                            
IN1      DTFMT BLKSIZE=8000,RECSIZE=80,TYPEFLE=INPUT,RECFORM=FIXBLK,   X        
               EOFADDR=INPUTEND,FILABL=STD,WORKA=YES,REWIND=UNLOAD,    X        
               DEVADDR=SYS005,IOAREA1=IN1A                                      
IN1A     DS    8000C                                                            
OUT      DTFMT DEVADDR=SYS007,BLKSIZE=8000,RECFORM=VARBLK,             X        
               TYPEFLE=OUTPUT,IOAREA1=OUT1,WORKA=YES,FILABL=STD,       X        
               REWIND=UNLOAD                                                    
OUT1     DS    8000C                                                            
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE RZSVIFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030SVIUPDATE 05/01/02'                                      
         END                                                                    
