*          DATA SET DMPRTQTS   AT LEVEL 002 AS OF 05/01/02                      
*PHASE PRTQT,*                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE GETRET                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE QSORT                                                                  
         TITLE 'PQTEST - TEST REMOTE PRINTING'                                  
         PRINT NOGEN                                                            
PQTEST   CSECT                                                                  
         NBASE 0,PQTEST,R9,WORK=A(PQWORK)                                       
         L     R2,=V(CPRINT)                                                    
         USING DPRINT,R2                                                        
         MVI   COLSMAX,132                                                      
         MVC   TITLE(30),=CL30'PROGRAM TO TEST DMPRTQ ROUTINE'                  
         LA    R3,Q                                                             
         USING PQPLD,R3                                                         
         LA    R4,BUFF                                                          
         USING PQRECD,R4                                                        
*                                                                               
         MVC   PRTQID,=CL8'PRTQU'  SET DEFAULT PRTQ AND USER                    
         MVC   USERID,U1                                                        
         EJECT                                                                  
NEXT     GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
*                                                                               
NEXT1    CLI   C,C'*'              IGNORE COMMENT CARDS                         
         BE    NEXT                                                             
         CLC   C(2),=C'/*'                                                      
         BE    EXIT                                                             
*                                                                               
NEXT2    MVC   P,SPACES            PRINT CARD                                   
         GOTO1 =V(PRINTER)                                                      
         MVI   P,C'='                                                           
         MVC   P+1(131),P                                                       
         GOTO1 =V(PRINTER)                                                      
         MVC   P(80),C                                                          
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
NEXT3    CLC   C(6),=C'DDSIO='     DDSIO=XXXXXX (MUST BE 1ST CARD)              
         BNE   NEXT4                                                            
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),C+6                                                      
         B     NEXT                                                             
*                                                                               
NEXT4    LA    RE,C+3              FI=PRTQN/PQ=PRTQN/PRTQN                      
         CLC   C(3),=C'FI='                                                     
         BE    NEXT4A                                                           
         CLC   C(3),=C'PQ='                                                     
         BE    NEXT4A                                                           
         LA    RE,C                                                             
         CLC   C(4),=C'PRTQ'                                                    
         BNE   NEXT4X                                                           
NEXT4A   MVC   PRTQID(5),0(RE)                                                  
         CLI   PRTQID+4,C'U'                                                    
         BE    NEXT4X                                                           
         CLI   PRTQID+4,C'1'                                                    
         BL    *+12                                                             
         CLI   PRTQID+4,C'8'                                                    
         BNH   NEXT4X                                                           
NEXT4B   MVC   P(30),=CL30'INVALID PRTQ FILE ID'                                
         GOTO1 =V(PRINTER)                                                      
         B     EXIT                                                             
NEXT4X   EQU   *                                                                
*                                                                               
NXT5     CLI   C+2,C'1'            3RD CHR OF ACTION CAN BE PRTQ NUM            
         BL    NXT5X                                                            
         CLI   C+2,C'8'                                                         
         BH    NXT5X                                                            
         MVC   PRTQID+4(1),C+2                                                  
NXT5X    EQU   *                                                                
*                                                                               
NXT6     CLC   C(2),=C'DIE'        FIRST TWO BYTES GIVES ACTION                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   C(2),=C'OLD'                                                     
         BE    OLD                                                              
         CLC   C(2),=C'NEW'                                                     
         BE    NEW                                                              
         CLC   C(2),=C'XTN'                                                     
         BE    NEW                                                              
         CLC   C(2),=C'RPL'                                                     
         BE    NEW                                                              
         CLC   C(2),=C'REA'                                                     
         BE    REA                                                              
         CLC   C(2),=C'RAN'                                                     
         BE    RAN                                                              
         CLC   C(2),=C'SEQ'                                                     
         BE    SEQ                                                              
         CLC   C(2),=C'NDX'                                                     
         BE    NDX                                                              
         CLC   C(2),=C'CHK'                                                     
         BE    CHK                                                              
         CLC   C(2),=C'STA'                                                     
         BE    STA                                                              
         CLC   C(2),=C'U1'                                                      
         BNE   *+14                                                             
         MVC   USERID,U1                                                        
         B     NEXT                                                             
         CLC   C(2),=C'U2'                                                      
         BNE   *+14                                                             
         MVC   USERID,U2                                                        
         B     NEXT                                                             
         B     NEXT                                                             
         SPACE 2                                                                
EXIT     XBASE                                                                  
         EJECT                                                                  
*              00000000001111111111222222222233333333334444                     
*              01234567890123456789012345678901234567890123                     
* CARD FORMAT  OLDLLLLPPPPIIIC                        TEXT.......               
*                                                                               
OLD      XC    Q,Q                                                              
         MVC   Q+1(11),SPACES                                                   
         MVC   Q+1(3),C+11         DESC                                         
         MVC   Q+12(3),C+11        SUBID                                        
         MVC   Q+17(2),USERID      DDS USER ID                                  
         MVC   Q+25(1),C+14        CLASS                                        
         PACK  DUB,C+3(4)                                                       
         CVB   R6,DUB                                                           
         STH   R6,LINES            R6=NUMBER OF LINES                           
         PACK  DUB,C+7(4)                                                       
         CVB   R7,DUB                                                           
         STH   R7,PAGES            R7=NUMBER OF PAGES                           
         ZAP   PAGNO,=P'0'                                                      
         BAS   RA,OLDPRT           OPEN THE REPORT                              
         LTR   R6,R6                                                            
         BZ    OLDCLO                                                           
         LTR   R7,R6                                                            
         BZ    OLDCLO                                                           
*                                                                               
OLDPAGEL MVI   Q,X'89'             NEXT PAGE LOOP                               
         MVC   Q+1(132),SPACES                                                  
         BAS   RA,OLDPRT                                                        
         AP    PAGNO,=P'1'                                                      
         ZAP   LINNO,=P'1'                                                      
         LH    R6,LINES                                                         
*                                                                               
OLDLINEL MVI   Q,X'09'             NEXT LINE LOOP                               
         UNPK  Q+1(4),PAGNO                                                     
         OI    Q+4,C'0'                                                         
         UNPK  Q+5(4),LINNO                                                     
         OI    Q+8,C'0'                                                         
         MVC   Q+9(32),C+39                                                     
         BAS   RA,OLDPRT                                                        
         AP    LINNO,=P'1'                                                      
         BCT   R6,OLDLINEL                                                      
         BCT   R7,OLDPAGEL                                                      
*                                                                               
OLDCLO   MVI   Q,X'FF'            CLOSE REPORT                                  
         BAS   RA,OLDPRT                                                        
         LR    RF,R4                                                            
         GOTO1 =V(HEXOUT),PARM,(RF),P,50,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
         LA    RF,50(R4)                                                        
         GOTO1 =V(HEXOUT),PARM,(RF),P,50,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
         LA    RF,100(R4)                                                       
         GOTO1 =V(HEXOUT),PARM,(RF),P,28,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
*                                                                               
OLDPRT   GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT',PRTQID,,Q,BUFF                      
         CLI   8(R1),0                                                          
         BER   RA                                                               
         DC    H'0'                                                             
         EJECT                                                                  
*              00000000001111111111222222222233333333334444                     
*              01234567890123456789012345678901234567890123                     
* CARD FORMAT  NEWLLLLPPPPIIIC  LLLDDDWWWCFCL         TEXT.......               
*                                                                               
NEW      XC    Q,Q                                                              
         MVI   QLEXTRA,X'FF'       SET NEW CALL EXTRA INFO FLAG                 
*                                                                               
XTN      CLC   C(2),=C'XT'                                                      
         BNE   RPL                                                              
         OC    LASTREPT(2),LASTREPT                                             
         BNZ   XTN1                                                             
         MVC   P(24),=C'NO LAST REPORT TO EXTEND'                               
         GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
XTN1     MVI   QLFLAG,QLFLRALL+QLFLKEY+QLFLXTND                                 
         MVC   QLKEY,LASTREPT                                                   
         B     NEW2                                                             
*                                                                               
RPL      CLC   C(2),=C'RP'                                                      
         BNE   NEW1                                                             
         OC    LASTREPT(2),LASTREPT                                             
         BNZ   RPL1                                                             
         MVC   P(25),=C'NO LAST REPORT TO REPLACE'                              
         GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
RPL1     MVI   QLFLAG,QLFLRALL+QLFLKEY                                          
         MVC   QLKEY,LASTREPT                                                   
         B     NEW2                                                             
*                                                                               
NEW1     MVC   QLDESC,SPACES                                                    
         MVC   QLDESC(3),C+11      DESC                                         
         MVC   QLSUBID,C+11        SUBID                                        
         MVC   QLSRCID,USERID      DDS USER ID                                  
         MVC   QLCLASS,C+14        CLASS                                        
         MVC   DUB1(3),C+17        RETN LIVE                                    
         OC    DUB1(3),=8C'0'                                                   
         PACK  DUB,DUB1(3)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,QLRETNL                                                     
         MVC   DUB1(3),C+20        RETN DEAD                                    
         OC    DUB1(3),=8C'0'                                                   
         PACK  DUB,DUB1(3)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,QLRETND                                                     
         MVC   DUB1(3),C+23        LINE WIDTH                                   
         OC    DUB1(3),=8C'0'                                                   
         PACK  DUB,DUB1(3)                                                      
         CVB   R0,DUB                                                           
         STC   R0,QLLINEW                                                       
         CLI   C+26,C' '           CC CHR                                       
         BE    *+8                                                              
         OI    QLLINET,QLLTCC                                                   
         CLI   C+27,C' '           FIXED LENGTH                                 
         BE    *+8                                                              
         OI    QLLINET,QLLTFL                                                   
         CLI   C+28,C' '           DATA IS COMPRESSABLE                         
         BE    *+8                                                              
         OI    QLLINET,QLLTDC                                                   
         CLI   C+29,C' '           LASER PRINTING CHRS                          
         BE    *+8                                                              
         OI    QLLINET,QLLTLA                                                   
         MVC   QLDESC,C+39                                                      
*                                                                               
NEW2     PACK  DUB,C+3(4)                                                       
         CVB   R6,DUB                                                           
         STH   R6,LINES            R6=NUMBER OF LINES                           
         PACK  DUB,C+7(4)                                                       
         CVB   R7,DUB                                                           
         STH   R7,PAGES            R7=NUMBER OF PAGES                           
*                                                                               
         LA    RF,=C'OPEN'         SET OPEN COMMAND                             
         BAS   RA,NEWPRT                                                        
         ZAP   PAGNO,=P'0'                                                      
         LTR   R6,R6                                                            
         BZ    NEWCLO                                                           
         LTR   R7,R7                                                            
         BZ    NEWCLO                                                           
*                                                                               
NEWPAGEL MVI   Q,X'89'             NEXT PAGE LOOP                               
         MVC   Q+1(132),SPACES                                                  
         LA    RF,=C'ADD'          SET ADD COMMAND                              
         BAS   RA,NEWPRT                                                        
         AP    PAGNO,=P'1'                                                      
         ZAP   LINNO,=P'1'                                                      
         LH    R6,LINES                                                         
*                                                                               
NEWLINEL MVI   Q,X'09'             NEXT LINE LOOP                               
         UNPK  Q+1(4),PAGNO                                                     
         OI    Q+4,C'0'                                                         
         UNPK  Q+5(4),LINNO                                                     
         OI    Q+8,C'0'                                                         
         MVC   Q+9(32),C+39                                                     
         LA    RF,=C'ADD'          SET ADD COMMAND                              
         BAS   RA,NEWPRT                                                        
         AP    LINNO,=P'1'                                                      
         BCT   R6,NEWLINEL                                                      
         BCT   R7,NEWPAGEL                                                      
*                                                                               
NEWCLO   LA    RF,=C'CLOSE'        SET CLOSE COMMAND                            
         MVI   Q,X'FF'                                                          
         BAS   RA,NEWPRT                                                        
         LR    RF,R4                                                            
         MVC   LASTREPT(7),0(R4)   SAVE REPORT KEY                              
         GOTO1 =V(HEXOUT),PARM,(RF),P,50,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
         LA    RF,50(R4)                                                        
         GOTO1 =V(HEXOUT),PARM,(RF),P,50,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
         LA    RF,100(R4)                                                       
         GOTO1 =V(HEXOUT),PARM,(RF),P,28,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
*                                                                               
NEWPRT   LA    R8,Q                POINT TO CC CHR                              
         CLI   C+26,C' '                                                        
         BNE   NEWPRT1                                                          
         CLC   0(3,RF),=C'ADD'                                                  
         BNE   NEWPRT1                                                          
         LA    R8,Q+1                                                           
         CLI   Q,X'89'                                                          
         BER   RA                                                               
NEWPRT1  GOTO1 =V(DATAMGR),DMCB,(RF),PRTQID,X,(R8),BUFF                         
         CLI   8(R1),0                                                          
         BER   RA                                                               
         DC    H'0'                                                             
         EJECT                                                                  
* CARD FORMAT  REAIIII WHERE IIII IS MAX RECORDS                                
*                                                                               
REA      MVC   DUB(4),C+3          GET MAXIMUM NUMBER OF RECORDS                
         OC    DUB(4),=C'0000'                                                  
         PACK  DUB1,DUB(4)                                                      
         CVB   R0,DUB1                                                          
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         LA    R0,1                                                             
*                                                                               
REALOOP  GOTO1 =V(DATAMGR),DMCB,(X'00',=C'REA'),PRTQID,X,Q,BUFF                 
         CLI   DMCB+8,0                                                         
         BE    REAPRT                                                           
         LA    R0,1                                                             
         TM    DMCB+8,X'40'                                                     
         BZ    *+6                                                              
         DC    H'0'                DIE IF DISK ERROR                            
         MVC   Q(132),SPACES                                                    
         TM    DMCB+8,X'80'                                                     
         BZ    *+14                                                             
         MVC   Q(30),=CL30'****** END-OF-FILE ******'                           
         B     REAPRT                                                           
         TM    DMCB+8,X'20'                                                     
         BZ    *+14                                                             
         MVC   Q(30),=CL30'****** REC NOT FOUND ******'                         
         B     REAPRT                                                           
         DC    H'0'                                                             
*                                                                               
REAPRT   LA    RF,Q                POINT TO RETURN DATA                         
         CLC   Q(7),=C'****** '                                                 
         BE    REAPRT1                                                          
         CLI   Q,C'A'                                                           
         BNL   REAPRT1             BUMP PAST CC CHR                             
         LA    RF,Q+1                                                           
REAPRT1  MVC   P(80),0(RF)                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         SH    R0,=H'1'                                                         
         BNZ   REALOOP                                                          
         B     NEXT                                                             
         EJECT                                                                  
* CARD FORMAT  RAN9999LINE OR RAN9999PAGE OR RAN9999BOTH9999                    
*                                                                               
RAN      MVC   DUB(4),C+3          GET LINE OR PAGE NUMBER                      
         OC    DUB(4),=C'0000'                                                  
         PACK  DUB1,DUB(4)                                                      
         CVB   R0,DUB1                                                          
         CLC   C+7(4),=C'LINE'     TEST LINE OR PAGE                            
         BE    RAN1                                                             
         CLC   C+7(4),=C'PAGE'                                                  
         BE    RAN1                                                             
         CLC   C+7(4),=C'BOTH'                                                  
         BNE   NEXT                                                             
         MVC   DUB(4),C+11         GET LINE WITHIN PAGE                         
         OC    DUB(4),=C'0000'                                                  
         PACK  DUB1,DUB(4)                                                      
         CVB   R1,DUB1                                                          
         ST    R1,Q+8              SET LINE WITHIN PAGE                         
*                                                                               
RAN1     ST    R0,Q                SET PAGE/LINE NUM REQUIRED                   
         MVC   Q+4(4),C+7                                                       
         GOTO1 =V(DATAMGR),DMCB,(X'00',=C'RAN'),PRTQID,X,Q,BUFF                 
         CLI   DMCB+8,0                                                         
         BE    RANPRT                                                           
         TM    DMCB+8,X'40'                                                     
         BZ    *+6                                                              
         DC    H'0'                DIE IF DISK ERROR                            
         MVC   Q(132),SPACES                                                    
         TM    DMCB+8,X'80'                                                     
         BZ    *+14                                                             
         MVC   Q(30),=CL30'****** END-OF-FILE ******'                           
         B     RANPRT                                                           
         TM    DMCB+8,X'20'                                                     
         BZ    *+14                                                             
         MVC   Q(30),=CL30'****** REC NOT FOUND ******'                         
         B     RANPRT                                                           
         DC    H'0'                                                             
*                                                                               
RANPRT   LA    RF,Q                POINT TO RETURN DATA                         
         CLC   Q(7),=C'****** '                                                 
         BE    RANPRT1                                                          
         CLI   Q,C'A'                                                           
         BNL   *+8                 BUMP PAST CC CHR                             
         LA    RF,Q+1                                                           
RANPRT1  MVC   P(80),0(RF)                                                      
         GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
         EJECT                                                                  
* CARD FORMAT  SEQ SSSNNNNC                                                     
*                                                                               
SEQ      XC    X,X                 CLEAR USER INDEX                             
         CLI   C+3,C'D'                                                         
         BNE   *+8                                                              
         OI    X+30,X'80'          SET DATA REQUIRED                            
         CLC   C+4(3),SPACES                                                    
         BE    SEQLOOP                                                          
         MVC   X(2),USERID         SET DDS USER ID                              
         MVC   X+2(3),C+4          SET REPORT ID                                
         MVC   DUB(4),C+7                                                       
         OC    DUB(4),=C'0000'                                                  
         PACK  DUB1,DUB(4)                                                      
         CVB   R0,DUB1                                                          
         LTR   R0,R0                                                            
         BZ    SEQLOOP                                                          
         STCM  R0,3,X+5            SET REPORT NUMBER                            
         CLI   C+11,C' '                                                        
         BE    SEQLOOP                                                          
         MVC   X+7(1),C+11         SET REPORT CLASS                             
*                                                                               
SEQLOOP  XC    FUL,FUL                                                          
         GOTO1 =V(DATAMGR),DMCB,(X'00',=C'SEQ'),PRTQID,X,Q,BUFF                 
         CLI   DMCB+8,0                                                         
         BE    SEQPRT                                                           
         ST    R1,FUL              SET EOF/ERR CONDITION                        
         TM    DMCB+8,X'40'                                                     
         BZ    *+6                                                              
         DC    H'0'                DIE IF DISK ERROR                            
         XC    QH(4),QH                                                         
         MVC   Q(132),SPACES                                                    
         TM    DMCB+8,X'80'                                                     
         BZ    *+14                                                             
         MVC   Q(30),=CL30'****** END-OF-FILE ******'                           
         B     SEQPRT                                                           
         TM    DMCB+8,X'10'                                                     
         BZ    *+14                                                             
         MVC   Q(30),=CL30'****** REC NOT FOUND ******'                         
         B     SEQPRT                                                           
         DC    H'0'                                                             
*                                                                               
SEQPRT   LA    RF,Q                POINT TO RETURN DATA                         
         MVC   P,SPACES                                                         
         CLC   Q(7),=CL30'****** END-OF-FILE ******'                            
         BE    SEQPRT4                                                          
         CLC   Q(5),SOFLAB                                                      
         BE    SEQPRT1                                                          
         CLC   Q(5),EOFLAB                                                      
         BNE   SEQPRT3                                                          
SEQPRT1  MVC   P+5(10),=C'**      **'                                           
         MVC   P+7(6),Q+2                                                       
         CLC   Q(5),EOFLAB                                                      
         BE    SEQPRT4+6                                                        
         LA    R8,38                                                            
         TM    X+30,X'80'          TEST IF DATA AS WELL MODE                    
         BZ    *+8                                                              
         LA    R8,60                                                            
         GOTO1 =V(HEXOUT),PARM,Q+10,P+16,(R8),=C'TOG'                           
         B     SEQPRT4+6                                                        
SEQPRT3  CLI   Q,C'A'                                                           
         BNL   *+8                 BUMP PAST CC CHR                             
         LA    RF,Q+1                                                           
*                                                                               
SEQPRT4  MVC   P+5(80),0(RF)       SET UP REC LEN AND DATA                      
         LH    R1,QH                                                            
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P(4),DUB                                                         
*                                                                               
SEQPRTX  GOTO1 =V(PRINTER)                                                      
         OC    FUL,FUL                                                          
         BZ    SEQLOOP                                                          
         B     NEXT                                                             
         EJECT                                                                  
* CARD FORMAT  NDX SSSNNNNC                                                     
*                                                                               
NDX      XC    X,X                 CLEAR USER INDEX                             
         CLC   C+4(3),SPACES                                                    
         BE    NDXLOOP                                                          
         MVC   X(2),USERID         SET DDS USER ID                              
         MVC   X+2(3),C+4          SET REPORT ID                                
         MVC   DUB(4),C+7                                                       
         OC    DUB(4),=C'0000'                                                  
         PACK  DUB1,DUB(4)                                                      
         CVB   R0,DUB1                                                          
         LTR   R0,R0                                                            
         BZ    NDXLOOP                                                          
         STCM  R0,3,X+5            SET REPORT NUMBER                            
         CLI   C+11,C' '                                                        
         BE    NDXLOOP                                                          
         MVC   X+7(1),C+11         SET REPORT CLASS                             
*                                                                               
NDXLOOP  XC    FUL,FUL                                                          
         GOTO1 =V(DATAMGR),DMCB,(X'08',=C'IND'),PRTQID,X,Q,BUFF                 
         CLI   DMCB+8,0                                                         
         BE    NDXPRT                                                           
         ST    R1,FUL              SET EOF/ERR CONDITION                        
         TM    DMCB+8,X'40'                                                     
         BZ    *+6                                                              
         DC    H'0'                DIE IF DISK ERROR                            
         MVC   P,SPACES                                                         
         TM    DMCB+8,X'80'        END OF INDEX                                 
         BO    NDXPRTX                                                          
         TM    DMCB+8,X'10'        REC NOT FOUND                                
         BZ    *+14                                                             
         MVC   P(30),=CL30'****** REC NOT FOUND ******'                         
         B     NDXPRTX                                                          
         DC    H'0'                                                             
*                                                                               
NDXPRT   MVC   LASTREPT(7),X       SAVE LAST REPORT LOCATED                     
         LA    RF,L'PQINDEX                                                     
         GOTO1 =V(HEXOUT),PARM,X,P,(RF),=C'TOG'                                 
         MVC   P+48(5),=C' NDX '                                                
         GOTO1 =V(HEXOUT),PARM,X+28,P+53,2,=C'TOG'                              
*                                                                               
NDXPRTX  GOTO1 =V(PRINTER)                                                      
         OC    FUL,FUL                                                          
         BZ    NDXLOOP                                                          
         B     NEXT                                                             
         EJECT                                                                  
* CARD FORMAT  CHK SSSNNNNC                                                     
*                                                                               
CHK      GOTO1 =V(DATAMGR),DMCB,=C'BUFF',PRTQID,X,Q,BUFF                        
         L     RF,=A(BUFF)                                                      
         MVC   CIDATA(40),12(RF)                                                
         LA    R1,L'PQINDEX                                                     
         STH   R1,CINDXLN                                                       
*                                                                               
CHK0     XC    X,X                 CLEAR USER INDEX                             
         CLC   C+4(3),SPACES                                                    
         BE    CHK1                                                             
         MVC   X(2),USERID         SET DDS USER ID                              
         MVC   X+2(3),C+4          SET REPORT ID                                
         MVC   DUB(4),C+7                                                       
         OC    DUB(4),=C'0000'                                                  
         PACK  DUB1,DUB(4)                                                      
         CVB   R0,DUB1                                                          
         LTR   R0,R0                                                            
         BZ    CHK1                                                             
         STCM  R0,3,X+5            SET REPORT NUMBER                            
         CLI   C+11,C' '                                                        
         BE    CHK1                                                             
         MVC   X+7(1),C+11         SET REPORT CLASS                             
*                                                                               
CHK1     L     R0,=A(CIATAB)       CLEAR CI ADDR TABLE                          
         ST    R0,CIATABA                                                       
         XC    CIATABN,CIATABN                                                  
         XC    CIATABP,CIATABP                                                  
         XC    CIATABL,CIATABL                                                  
         L     R1,=A(CIATABX)                                                   
         SR    R1,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
CHKLOOP  XC    FUL,FUL                                                          
         GOTO1 =V(DATAMGR),DMCB,(X'08',=C'IND'),PRTQID,X,Q,BUFF                 
         CLI   DMCB+8,0                                                         
         BE    CHKPRT                                                           
         ST    R1,FUL              SET EOF/ERR CONDITION                        
         TM    DMCB+8,X'40'                                                     
         BZ    *+6                                                              
         DC    H'0'                DIE IF DISK ERROR                            
         MVC   P,SPACES                                                         
         TM    DMCB+8,X'80'        END OF INDEX                                 
         BO    CHKPRTA                                                          
         TM    DMCB+8,X'10'        REC NOT FOUND                                
         BZ    *+14                                                             
         MVC   P(30),=CL30'****** REC NOT FOUND ******'                         
         B     CHKPRTA                                                          
         DC    H'0'                                                             
*                                                                               
CHKPRT   LA    RF,L'PQINDEX                                                     
         GOTO1 =V(HEXOUT),PARM,X,P,(RF),=C'TOG'                                 
         MVC   P+48(5),=C' NDX '                                                
         GOTO1 =V(HEXOUT),PARM,X+28,P+53,2,=C'TOG'                              
         GOTO1 =V(PRINTER)                                                      
         MVC   DSKADR(2),X+28                                                   
         MVC   DSKADR+2(2),=X'0100'                                             
         L     R5,=A(BUFF1)                                                     
         USING PQRECD,R5                                                        
CHKPRT1  GOTO1 =V(DATAMGR),DMCB1,=C'DMREAD',PRTQID,DSKADR,(R5)                  
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RF,L'PQINDEX                                                     
         GOTO1 =V(HEXOUT),PARM,(R5),P,(RF),=C'TOG'                              
         SR    R0,R0                                                            
         IC    R0,PQSEQ                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   P+48(5),=C' ... '                                                
         UNPK  P+49(3),DUB+6(2)                                                 
         GOTO1 =V(HEXOUT),PARM,PQBATTR,P+53,16,=C'TOG'                          
         CLI   PQSEQ,1                                                          
         BH    CHKPRT1A                                                         
         GOTO1 =V(HEXOUT),PARM,PQDATA,P+86,18,=C'TOG'                           
         MVC   P+123(8),PQPRSYM                                                 
         OC    P+123(8),SPACES                                                  
CHKPRT1A CLC   X(7),0(R5)                                                       
         BE    CHKPRT2                                                          
         MVI   P+48,C'*'                                                        
         MVI   P+52,C'1'                                                        
CHKPRT2  GOTO1 =V(PRINTER)                                                      
CHKPRT3  OC    PQCINEXT,PQCINEXT                                                
         BZ    CHKPRT4                                                          
         MVC   DSKADR(2),PQCINEXT                                               
         L     RE,CIATABA                                                       
         CLC   0(2,RE),=X'FFFF'    TEST IF SPACE IN TABLE                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   0(2,RE),DSKADR      MOVE PART2 CI ADDR TO TABLE                  
         LA    RE,2(RE)                                                         
         ST    RE,CIATABA                                                       
         L     RE,CIATABN          BUMP NUMBER OF TABLE ENTRIES                 
         LA    RE,1(RE)                                                         
         ST    RE,CIATABN                                                       
         B     CHKPRT1                                                          
CHKPRT4  OC    FUL,FUL                                                          
         BZ    CHKLOOP                                                          
*                                                                               
CHKPRTA  GOTO1 =V(PRINTER)         END OF INDEX                                 
         OC    FUL,FUL                                                          
         BZ    CHKLOOP                                                          
*                                                                               
CHKPRTB  XC    DMCB(24),DMCB       SORT LIST IF PART2 CI ADDRS                  
         L     R6,=A(CIATAB)                                                    
         ST    R6,DMCB             R6=A(FIRST ENTRY)                            
         ICM   R0,15,CIATABN                                                    
         BZ    NEXT                                                             
         ST    R0,DMCB+4           R0=NUM OF ENTRIES                            
         MVC   DMCB+8(4),=F'2'     LEN OF RECORD                                
         MVC   DMCB+12(4),=F'2'    LEN OF KEY                                   
         GOTO1 =V(QSORT),DMCB                                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
CHK2     BAS   RE,CXLOOPJ          POINT TO START OF PART2 INDEX                
         USING PQRECD,R5           R5=A(PRTQUE INDEX ENTRY)                     
         L     R6,=A(CIATAB)                                                    
*                                                                               
CHK2A    BAS   RE,GETXAD                                                        
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),PRTQID,CXADDR,CXREC              
         CLI   8(R1),0                                                          
         BE    CHK2B                                                            
         DC    H'0'                                                             
*                                                                               
CHK2B    BAS   RE,GETCAD           SET CIADDR TO CI DISK ADDRESS                
         CLC   CIADDR(2),0(R6)     IS CIADDR NEXT IN TABLE                      
         BNE   CHK2D               NO                                           
         CLI   PQSTAT,PQSTPU       YES CANT BE PURGED                           
         BE    CHK2C                                                            
         LA    R6,2(R6)            THIS IS OK SO BUMP TO NEXT                   
         B     CHK2N                                                            
*                                                                               
CHK2C    EQU   *                   ERROR PURGED PART2 IN TABLE                  
         LA    RF,L'PQINDEX                                                     
         GOTO1 =V(HEXOUT),PARM,(R5),P,(RF),=C'TOG'                              
         MVC   P+48(5),=C'*NDX2'                                                
         GOTO1 =V(HEXOUT),PARM,CIADDR,P+53,2,=C'TOG'                            
         GOTO1 =V(PRINTER)                                                      
         LA    R6,2(R6)                                                         
         B     CHK2N                                                            
*                                                                               
CHK2D    CLI   PQSTAT,PQSTPU       CIADDR NOT IN TABLE                          
         BNE   CHK2E               THIS IS OK IF ITS PURGED                     
         L     R1,CIATABP                                                       
         LA    R1,1(R1)                                                         
         ST    R1,CIATABP          BUMP PURGED PART2 CIS                        
         B     CHK2N                                                            
*                                                                               
CHK2E    EQU   *                   ERROR NON PURGED PART2 NOT IN TAB            
         LA    RF,L'PQINDEX                                                     
         GOTO1 =V(HEXOUT),PARM,(R5),P,(RF),=C'TOG'                              
         MVC   P+48(5),=C'*NDX3'                                                
         GOTO1 =V(HEXOUT),PARM,CIADDR,P+53,2,=C'TOG'                            
         GOTO1 =V(PRINTER)                                                      
         L     R1,CIATABL                                                       
         LA    R1,1(R1)                                                         
         ST    R1,CIATABL          BUMP LOST PART2 CIS                          
         B     CHK2N                                                            
*                                                                               
CHK2N    BAS   RE,CXLOOPX          BUMP TO NEXT INDEX ENTRY                     
         B     CHK2B                                                            
         B     CHK2A               END OF PAGE                                  
*                                                                               
         GOTO1 =V(PRINTER)                                                      
         L     R0,CIATABN                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P(4),DUB                                                         
         L     R0,CIATABP                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+6(4),DUB                                                       
         L     R0,CIATABL                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+12(4),DUB                                                      
         GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
*                                                                               
         EJECT                                                                  
* CARD FORMAT  STA SSSNNNNC XXX                                                 
*                                                                               
STA      XC    X,X                 CLEAR USER INDEX                             
         CLC   C+4(8),SPACES                                                    
         BE    STAIND              NO REPORT SPECIFIED ASSUME DEFINED           
         CLC   C+4(3),SPACES                                                    
         BE    STA1                                                             
         CLC   C+4(3),=C'ALL'                                                   
         BE    STA1                                                             
         MVC   X(2),USERID         SET DDS USER ID                              
         MVC   X+2(3),C+4          SET REPORT ID                                
         MVC   DUB(4),C+7                                                       
         OC    DUB(4),=C'0000'                                                  
         PACK  DUB1,DUB(4)                                                      
         CVB   R0,DUB1                                                          
         LTR   R0,R0                                                            
         BZ    STA1                                                             
         STCM  R0,3,X+5            SET REPORT NUMBER                            
         CLI   C+11,C' '                                                        
         BE    STA1                                                             
         MVC   X+7(1),C+11         SET REPORT CLASS                             
STA1     EQU   *                                                                
*                                                                               
STALOOP  XC    FUL,FUL                                                          
         GOTO1 =V(DATAMGR),DMCB,(X'08',=C'IND'),PRTQID,X,Q,BUFF                 
         CLI   DMCB+8,0                                                         
         BE    STALOOP2                                                         
*                                                                               
STALOOP1 ST    R1,FUL              SET EOF/ERR CONDITION                        
         TM    DMCB+8,X'40'                                                     
         BZ    *+6                                                              
         DC    H'0'                DIE IF DISK ERROR                            
         MVC   P,SPACES                                                         
         TM    DMCB+8,X'80'                                                     
         BZ    *+14                                                             
         MVC   P(30),=CL30'****** END-OF-FILE ******'                           
         B     STAPRT                                                           
         TM    DMCB+8,X'10'                                                     
         BZ    *+14                                                             
         MVC   P(30),=CL30'****** REC NOT FOUND ******'                         
         B     STAPRT                                                           
         DC    H'0'                                                             
*                                                                               
STALOOP2 XC    FUL,FUL                                                          
         LA    R8,32                                                            
         GOTO1 =V(HEXOUT),PARM,X,P,(R8),=C'TOG'                                 
         GOTO1 =V(DATAMGR),DMCB,(X'00',C+13),PRTQID,X,Q,BUFF                    
         CLI   DMCB+8,0                                                         
         BNE   STALOOP1                                                         
         MVC   P+70(30),=CL30'STATUS CHANGED TO XXX'                            
         MVC   P+88(3),C+13                                                     
*                                                                               
STAPRT   GOTO1 =V(PRINTER)                                                      
         OC    FUL,FUL                                                          
         BNZ   NEXT                                                             
         CLC   C+4(3),=C'ALL'                                                   
         BE    STALOOP                                                          
         B     NEXT                                                             
*                                                                               
STAIND   EQU   *                                                                
         GOTO1 =V(DATAMGR),DMCB,(X'00',C+13),PRTQID,X,Q,BUFF                    
         CLI   DMCB+8,0                                                         
         BNE   STAIND1                                                          
         LA    R8,L'PQINDEX                                                     
         GOTO1 =V(HEXOUT),PARM,BUFF,P,(R8),=C'TOG'                              
         MVC   P+70(30),=CL30'STATUS CHANGED TO XXX'                            
         MVC   P+88(3),C+13                                                     
STAIND1  GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
         EJECT                                                                  
* STANDARD DMPRTQR ROUTINES                                                     
*                                                                               
       ++INCLUDE DMPRTQR                                                        
         EJECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
DUB2     DS    D                                                                
FUL      DS    F                                                                
DSKADR   DS    F                                                                
CIATABN  DS    F                                                                
CIATABP  DS    F                                                                
CIATABL  DS    F                                                                
CIATABA  DS    A                                                                
P1       DS    6F                                                               
DMCB     DS    6F                                                               
DMCB1    DS    6F                                                               
PARM     DS    6F                                                               
PRTQID   DS    CL8                                                              
LASTREPT DC    XL8'00'                                                          
*                                                                               
*DMPRTQW                                                                        
       ++INCLUDE DMPRTQW                                                        
*                                                                               
LINES    DS    H                                                                
PAGES    DS    H                                                                
LINNO    DS    PL4                                                              
PAGNO    DS    PL4                                                              
*                                                                               
C        DS    CL80                                                             
*                                                                               
SOFLAB   DC    X'0000',C'SOFSOF',X'0000'                                        
EOFLAB   DC    X'FFFF',C'EOFEOF',X'FFFF'                                        
*                                                                               
USERID   DS    H                                                                
*&&UK                                                                           
U1       DC    H'33'               DDS2...ODD                                   
U2       DC    H'38'               DDS1...EVEN                                  
*&&                                                                             
*&&US                                                                           
U1       DC    H'43'               TCH1...ODD                                   
U2       DC    H'42'               ????...EVEN                                  
*&&                                                                             
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'*NDXNDX*'                                                      
X        DC    XL40'00'                                                         
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'*LNELNE*'                                                      
QH       DC    XL4'00'                                                          
Q        DC    CL133' '                                                         
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'*SOBSOB*'                                                      
CXREC    DS    0C                                                               
BUFF     DC    14336X'00'                                                       
         DC    C'*EOBEOB*'                                                      
BUFF1    DC    14336X'00'                                                       
         SPACE 2                                                                
         DC    C'*CIATAB*'                                                      
CIATAB   DS    20000C                                                           
CIATABX  DC    8X'FF'                                                           
         SPACE 2                                                                
         DC    C'*WRKWRK*'                                                      
PQWORK   DS    8000D                                                            
         SPACE 2                                                                
UTL      CSECT                                                                  
         DC    F'0',X'01'                                                       
SSB      CSECT                                                                  
         DC    10F'0'                                                           
         EJECT                                                                  
*DMPRTQD                                                                        
       ++INCLUDE DMPRTQD                                                        
         EJECT                                                                  
*DMPRTQK                                                                        
       ++INCLUDE DMPRTQK                                                        
         EJECT                                                                  
*DMPRTQL                                                                        
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
*DMPRTQS                                                                        
       ++INCLUDE DMPRTQS                                                        
         EJECT                                                                  
*DDDPRINTL                                                                      
       ++INCLUDE DDDPRINTL                                                      
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DMPRTQTS  05/01/02'                                      
         END                                                                    
