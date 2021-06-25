*          DATA SET DMPRTQT    AT LEVEL 009 AS OF 12/15/16                      
*PHASE PRTQTA                                                                   
*INCLUDE DMDMGRL                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE QSORT                                                                  
         TITLE 'PQTEST - TEST REMOTE PRINTING'                                  
         PRINT NOGEN                                                            
PQTEST   CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         ENTRY MASTC                                                            
*                                                                               
         NBASE 0,PQTEST,R9,WORK=A(PQWORK)                                       
         L     R2,=V(CPRINT)                                                    
         USING DPRINT,R2                                                        
         MVI   COLSMAX,132                                                      
         MVC   TITLE(24),=CL24'TEST PRINT QUEUE ACTIONS'                        
*                                                                               
PQTEST1  LA    R3,Q                R3=A(PQ PRINT LINE)                          
         USING PQPLD,R3                                                         
         L     R4,=A(BUFF)         R4=A(PQ BUFFER)                              
         USING PQRECD,R4                                                        
         MVC   USERID,=H'6014'     SET DEFAULT USER ID                          
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
         BNE   NEXT3A                                                           
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),C+6                                                      
         B     NEXT                                                             
*                                                                               
NEXT3A   CLC   C(7),=C'DSPACE='    DSPACE=                                      
         BNE   NEXT3B                                                           
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(1,RF),C+7                                        
         B     NEXT                                                             
*                                                                               
NEXT3B   CLC   C(4),=C'ARC='                                                    
         BNE   NEXT3C                                                           
         L     R1,=A(MCARC)                                                     
         MVC   0(72,R1),C          SAVE ARC=CARD IN MCARC                       
         B     NEXT                                                             
*                                                                               
NEXT3C   CLC   C(6),=C'PRINT='     PRINT=Y/N/E - YES/NO/ERRORS                  
         BNE   NEXT3D                                                           
         MVC   PFLAG,C+6                                                        
         B     NEXT                                                             
*                                                                               
NEXT3D   CLC   C(6),=C'WRSRV='     WRITE=Y/N                                    
         BE    NEXT3D1                                                          
         CLC   C(6),=C'WRITE='     WRITE=Y/N                                    
         BNE   NEXT3E                                                           
NEXT3D1  MVC   WFLAG,C+6                                                        
         CLI   WFLAG,C'Y'                                                       
         BE    NEXT                                                             
         CLI   WFLAG,C'N'                                                       
         BNE   NEXT3D2                                                          
         L     RF,=A(SSB)                                                       
         OI    SSOMTIND-SSOOFF(RF),SSOWSRN  SERVICE WRITE=NO                    
         B     NEXT                                                             
NEXT3D2  MVC   P(15),=CL15'*** INVALID ***'                                     
         GOTO1 =V(PRINTER)                                                      
         B     EXIT                                                             
*                                                                               
NEXT3E   CLC   C(8),=C'CLEANUP='   CLEANUP=PRTQX NN WRITE=NO                    
         BNE   NEXT4                                                            
         LA    RF,X                                                             
         USING UKRECD,X                                                         
         XC    UKINDEX,UKINDEX                                                  
         OC    C+14(2),ZEROS       NUMBER OF HOURS TO GO BACK                   
         PACK  DUB,C+14(2)                                                      
         CVB   R0,DUB                                                           
         STH   R0,UKINFO                                                        
*                                                                               
         MVC   UPDATE,WFLAG        SET TO VALUE IN WRITE=Y/N CARD               
         CLC   C+17(6),=C'WRITE='                                               
         BNE   *+10                                                             
         MVC   UPDATE,C+23         CAN BE MODIFIED ON THIS CARD                 
         CLI   UPDATE,C' '                                                      
         BNE   *+8                                                              
         MVI   UPDATE,C'Y'         THIS DEFAULTS TO UPDATIVE                    
         CLI   UPDATE,C'N'                                                      
         BNE   *+10                                                             
         MVC   UKUSRINF(8),=C'WRITE=NO' PASS AREA FOR WRITE=NO                  
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(X'00',=C'CLEANUP'),C+8,X,Q,BUFF                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RF,X                                                             
         MVC   P(25),=CL25'PART#1=00000 PART#2=00000'                           
         SR    R0,R0                                                            
         ICM   R0,3,UKUSRINF+0     GET PART#1 PURGED COUNT                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+07(5),DUB                                                      
         ICM   R0,3,UKUSRINF+2     GET PART#2 PURGED COUNT                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+20(5),DUB                                                      
         GOTO1 =V(PRINTER)                                                      
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
         CLI   PRTQID+4,C'A'                                                    
         BL    NEXT4B                                                           
         CLI   PRTQID+4,C'G'                                                    
         BNH   NEXT4X                                                           
         CLI   PRTQID+4,C'1'                                                    
         BL    NEXT4B                                                           
         CLI   PRTQID+4,C'9'                                                    
         BNH   NEXT4X                                                           
NEXT4B   MVC   P(30),=CL30'INVALID PRTQ FILE ID'                                
         GOTO1 =V(PRINTER)                                                      
         B     EXIT                                                             
NEXT4X   EQU   *                                                                
*                                                                               
NXT5     CLI   C+2,C'1'            3RD CHR OF ACTION CAN BE PRTQ NUM            
         BL    NXT5X                                                            
         CLI   C+2,C'9'                                                         
         BH    NXT5X                                                            
         MVC   PRTQID+4(1),C+2                                                  
NXT5X    EQU   *                                                                
*                                                                               
NXT6     CLC   C(2),=C'DIE'        FIRST TWO BYTES GIVES ACTION                 
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
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
         CLC   C(2),=C'STA'                                                     
         BE    STA                                                              
         CLC   C(2),=C'SCR'                                                     
         BE    SCR                                                              
         CLC   C(2),=C'TST'                                                     
         BE    TST                                                              
*                                                                               
         CLC   C(2),=C'CHK'                                                     
         BNE   *+14                                                             
         L     RF,=A(CHK)                                                       
         BASR  RE,RF                                                            
         B     NEXT                                                             
*                                                                               
         CLC   C(2),=C'ANA'                                                     
         BNE   *+14                                                             
         L     RF,=A(ANA)                                                       
         BASR  RE,RF                                                            
         B     NEXT                                                             
*                                                                               
         CLC   C(2),=C'U1'                                                      
         BNE   *+14                                                             
         MVC   USERID,U1                                                        
         B     NEXT                                                             
         CLC   C(2),=C'U2'                                                      
         BNE   *+14                                                             
         MVC   USERID,U2                                                        
         B     NEXT                                                             
         CLC   C(2),=C'UO'                                                      
         BNE   *+14                                                             
         MVC   USERID,UO                                                        
         B     NEXT                                                             
         CLC   C(2),=C'UE'                                                      
         BNE   *+14                                                             
         MVC   USERID,UE                                                        
         B     NEXT                                                             
         CLC   C(2),=C'U='                                                      
         BNE   *+14                                                             
         MVC   USERID,C+2                                                       
         B     NEXT                                                             
*                                                                               
         B     NEXT                                                             
                                                                                
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
OLDPRT   LA    R8,Q                                                             
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT',PRTQID,,(R8),BUFF                   
         CLI   8(R1),0                                                          
         BER   RA                                                               
         L     RF,=A(CHKERR)                                                    
         BASR  RE,RF                                                            
         BR    RA                                                               
         EJECT                                                                  
*              00000000001111111111222222222233333333334444                     
*              01234567890123456789012345678901234567890123                     
* CARD FORMAT  NEWLLLLPPPPIIIC..LLLDDDWWWCFCLTA......XTEXT.......               
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
         MVC   QLREPTY,C+30        REPORT TYPE                                  
         CLI   C+31,C'A'                                                        
         BNE   *+8                                                              
         OI    QLTYP1,QLTYAR       ARCHIVABLE                                   
         CLI   C+31,C'E'                                                        
         BNE   *+8                                                              
         OI    QLTYP1,QLTYAE       ARCHIVE ELIGIBLE                             
*                                  XML IF C+38=X                                
         MVC   QLDESC,C+39         DESCRIPTION AND TEXT                         
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
         CLI   C+38,C'X'           TEST PUT XML STATEMENT AS LINE 1             
         BNE   NEWLINEL                                                         
         MVI   Q,X'09'                                                          
         MVC   Q+1(10),=C'<XML HERE>'                                           
         LA    RF,=C'ADD'          SET ADD COMMAND                              
         BAS   RA,NEWPRT                                                        
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
         GOTO1 =V(HEXOUT),PARM,(RF),P,50,=C'TOG'                                
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
         L     RF,=A(CHKERR)                                                    
         BASR  RE,RF                                                            
         BR    RA                                                               
         EJECT                                                                  
* CARD FORMAT  REAIIII WHERE IIII IS MAX RECORDS                                
*                                                                               
REA      MVC   DUB(4),C+3          GET MAXIMUM NUMBER OF RECORDS                
         OC    DUB(4),ZEROS                                                     
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
         MVC   Q(30),=CL30'*** END-OF-FILE ***'                                 
         B     REAPRT                                                           
         TM    DMCB+8,X'20'                                                     
         BZ    *+14                                                             
         MVC   Q(30),=CL30'*** REC NOT FOUND ***'                               
         B     REAPRT                                                           
         DC    H'0'                                                             
*                                                                               
REAPRT   LA    RF,Q                POINT TO RETURN DATA                         
         CLC   Q(4),=C'*** '                                                    
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
                                                                                
*----------------------------------------------------------------------         
* CARD FORMAT  RAN 9999LINE OR RAN 9999PAGE9999                                 
*----------------------------------------------------------------------         
RAN      MVC   DUB(4),C+4          GET LINE OR PAGE NUMBER                      
         OC    DUB(4),ZEROS                                                     
         PACK  DUB1,DUB(4)                                                      
         CVB   R0,DUB1                                                          
         CLC   C+8(4),=C'LINE'     TEST LINE OR PAGE                            
         BE    RAN1                                                             
         CLC   C+8(4),=C'PAGE'                                                  
         BNE   NEXT                                                             
         CLC   C+12(4),SPACES                                                   
         BNE   *+10                                                             
         MVC   C+12(4),=C'0001'                                                 
         MVC   DUB(4),C+12         GET LINE WITHIN PAGE                         
         OC    DUB(4),ZEROS                                                     
         PACK  DUB1,DUB(4)                                                      
         CVB   R1,DUB1                                                          
         STCM  R1,15,Q+8           SET LINE WITHIN PAGE                         
*                                                                               
RAN1     STCM  R0,15,Q             SET PAGE/LINE NUM REQUIRED                   
         MVC   Q+4(4),C+8                                                       
         GOTO1 =V(DATAMGR),DMCB,(X'00',=C'RAN'),PRTQID,X,Q,BUFF                 
         CLI   DMCB+8,0                                                         
         BE    RANPRT                                                           
         TM    DMCB+8,X'40'                                                     
         BZ    *+6                                                              
         DC    H'0'                DIE IF DISK ERROR                            
         MVC   Q(132),SPACES                                                    
         TM    DMCB+8,X'80'                                                     
         BZ    *+14                                                             
         MVC   Q(30),=CL30'*** END-OF-FILE ***'                                 
         B     RANPRT                                                           
         TM    DMCB+8,X'20'                                                     
         BZ    *+14                                                             
         MVC   Q(30),=CL30'*** REC NOT FOUND ***'                               
         B     RANPRT                                                           
         DC    H'0'                                                             
*                                                                               
RANPRT   LA    RF,Q                POINT TO RETURN DATA                         
         CLC   Q(4),=C'*** '                                                    
         BE    RANPRT1                                                          
         CLI   Q,C'A'                                                           
         BNL   *+8                 BUMP PAST CC CHR                             
         LA    RF,Q+1                                                           
RANPRT1  MVC   P(80),0(RF)                                                      
         GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
         EJECT                                                                  
                                                                                
*----------------------------------------------------------------------         
* CARD FORMAT  SEQ SSSNNNNNC                                                    
*----------------------------------------------------------------------         
SEQ      XC    X,X                 CLEAR USER INDEX                             
         CLI   C+3,C'D'                                                         
         BNE   *+8                                                              
         OI    X+30,X'80'          SET DATA REQUIRED                            
         CLC   C+4(3),SPACES                                                    
         BE    SEQLOOP                                                          
         MVC   X(2),USERID         SET DDS USER ID                              
         MVC   X+2(3),C+4          SET REPORT ID                                
         MVC   DUB(5),C+7                                                       
         OC    DUB(5),ZEROS                                                     
         PACK  DUB1,DUB(5)                                                      
         CVB   R0,DUB1                                                          
         LTR   R0,R0                                                            
         BZ    SEQLOOP                                                          
         STCM  R0,3,X+5            SET REPORT NUMBER                            
         CLI   C+12,C' '                                                        
         BE    SEQLOOP                                                          
         MVC   X+7(1),C+12         SET REPORT CLASS                             
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
         MVC   Q(30),=CL30'*** END-OF-FILE ***'                                 
         B     SEQPRT                                                           
         TM    DMCB+8,X'10'                                                     
         BZ    *+14                                                             
         MVC   Q(30),=CL30'*** REC NOT FOUND ***'                               
         B     SEQPRT                                                           
         DC    H'0'                                                             
*                                                                               
SEQPRT   LA    RF,Q                POINT TO RETURN DATA                         
         MVC   P,SPACES                                                         
         CLC   Q(7),=CL30'*** END-OF-FILE ***'                                  
         BE    SEQPRT4                                                          
         CLC   Q(4),SOFLAB                                                      
         BE    SEQPRT1                                                          
         CLC   Q(4),OSOFLAB                                                     
         BE    SEQPRT1                                                          
         CLC   Q(4),EOFLAB                                                      
         BE    SEQPRT1                                                          
         CLC   Q(4),OEOFLAB                                                     
         BNE   SEQPRT3                                                          
SEQPRT1  MVC   P+5(5),=C'*   *'                                                 
         MVC   P+6(3),Q+1                                                       
         CLC   Q(4),EOFLAB                                                      
         BE    SEQPRT4+6                                                        
         CLC   Q(4),OEOFLAB                                                     
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
* CARD FORMAT  NDX SSSNNNNNC                                                    
*                                                                               
NDX      XC    X,X                 CLEAR USER INDEX                             
         CLC   C+4(3),SPACES                                                    
         BE    NDX1                                                             
         MVC   X(2),USERID         SET DDS USER ID                              
         MVC   X+2(3),C+4          SET REPORT ID                                
         MVC   DUB(5),C+7                                                       
         OC    DUB(5),ZEROS                                                     
         PACK  DUB1,DUB(5)                                                      
         CVB   R0,DUB1                                                          
         LTR   R0,R0                                                            
         BZ    NDX1                                                             
         STCM  R0,3,X+5            SET REPORT NUMBER                            
         CLI   C+3,C'='            TEST NDX= FOR DIRECT INDEX LOCATE            
         BNE   *+8                                                              
         OI    X+UKFLAG-UKKEY,X'04'                                             
         CLI   C+3,C'+'            TEST NDX+ FOR EXTRA INDEX DATA               
         BNE   *+8                                                              
         OI    X+UKFLAG1-UKKEY,X'80'                                            
         CLI   C+3,C'#'            TEST NDX# FOR DIRECT AND EXTRA               
         BNE   *+12                                                             
         OI    X+UKFLAG-UKKEY,X'04'                                             
         OI    X+UKFLAG1-UKKEY,X'80'                                            
         CLI   C+3,C'/'            TEST NDX/ FOR DIRECT AND CIADDR              
         BNE   *+8                                                              
         OI    X+UKFLAG-UKKEY,X'06'                                             
         CLI   C+3,C'\'            TEST NDX\ FOR DIRECT/CIADDR/READ             
         BNE   *+8                                                              
         OI    X+UKFLAG-UKKEY,X'07'                                             
         CLI   C+3,C'-'            TEST NDX\ FOR DIRECT/CIADDR/READ             
         BNE   *+8                                                              
         OI    X+UKFLAG-UKKEY,0                                                 
         CLI   C+12,C' '                                                        
         BE    NDX1                                                             
         MVC   X+7(1),C+12         SET REPORT CLASS                             
NDX1     MVC   INREPT,X            SAVE INPUT REPORT DEFINITION                 
*                                                                               
NDXLOOP  XC    FUL,FUL             SET NO ERROR                                 
         GOTO1 =V(DATAMGR),DMCB,(X'08',=C'IND'),PRTQID,X,Q,BUFF                 
         CLI   DMCB+8,0                                                         
         BE    NDXPRT                                                           
         ST    R1,FUL              SET EOF/ERR CONDITION                        
         TM    DMCB+8,X'40'                                                     
         BZ    *+6                                                              
         DC    H'0'                DIE IF DISK ERROR                            
         MVC   P,SPACES                                                         
         TM    DMCB+8,X'80'        END OF INDEX                                 
         BZ    *+14                                                             
         MVC   P(30),=CL30'*** END-OF-PART#1 INDEX ***'                         
         B     NDXPRTX                                                          
         TM    DMCB+8,X'10'        REC NOT FOUND                                
         BZ    *+14                                                             
         MVC   P(30),=CL30'*** INDEX REC NOT FOUND ***'                         
         B     NDXPRTX                                                          
         DC    H'0'                                                             
*                                                                               
NDXPRT   MVC   LASTREPT(7),X       SAVE LAST REPORT LOCATED                     
         TM    X+09,X'80'          TEST IF SECURE REPORT                        
         BZ    *+8                                                              
         MVI   P+48,C'>'                                                        
         LA    RF,24                                                            
         GOTO1 =V(HEXOUT),PARM,X+00,P,(RF),=C'TOG'                              
         MVC   P+49(3),=C'NDX'                                                  
         GOTO1 =V(HEXOUT),PARM,X+27,P+53,3,=C'TOG'                              
         TM    X+UKFLAG1-UKKEY,X'80'                                            
         BZ    NDXPRTX                                                          
         GOTO1 =V(HEXOUT),PARM,X+40,P+60,16,=C'TOG'                             
*                                                                               
NDXPRTX  GOTO1 =V(PRINTER)                                                      
         CLC   LASTREPT(7),INREPT  EXACT MATCH ON INPUT REPORT                  
         BE    NEXT                                                             
         OC    FUL,FUL                                                          
         BZ    NDXLOOP                                                          
         B     NEXT                                                             
         EJECT                                                                  
* CARD FORMAT  STA SSSNNNNNC XXX                                                
*                                                                               
STA      CLC   C+4(8),SPACES       NO REPORT SPECIFIED ASSUME DEFINED           
         BE    STAIND                                                           
         XC    X,X                                                              
         CLC   C+4(3),SPACES                                                    
         BE    STA1                                                             
         CLC   C+4(3),=C'ALL'                                                   
         BE    STA1                                                             
         MVC   X(2),USERID         SET DDS USER ID                              
         MVC   X+2(3),C+4          SET REPORT ID                                
         MVC   DUB(5),C+7                                                       
         OC    DUB(5),ZEROS                                                     
         PACK  DUB1,DUB(5)                                                      
         CVB   R0,DUB1                                                          
         LTR   R0,R0                                                            
         BZ    STA1                                                             
         STCM  R0,3,X+5            SET REPORT NUMBER                            
         CLI   C+12,C' '                                                        
         BE    STA1                                                             
         MVC   X+7(1),C+12         SET REPORT CLASS                             
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
         MVC   P(30),=CL30'*** END-OF-INDEX ***'                                
         B     STAPRT                                                           
         TM    DMCB+8,X'10'                                                     
         BZ    *+14                                                             
         MVC   P(30),=CL30'*** INDEX REC NOT FOUND ***'                         
         B     STAPRT                                                           
         DC    H'0'                                                             
*                                                                               
STALOOP2 XC    FUL,FUL                                                          
         LA    R8,32                                                            
         GOTO1 =V(HEXOUT),PARM,X,P,(R8),=C'TOG'                                 
         GOTO1 =V(DATAMGR),DMCB,(X'00',C+14),PRTQID,X,Q,BUFF                    
         CLI   DMCB+8,0                                                         
         BNE   STALOOP1                                                         
         MVC   P+70(30),=CL30'STATUS CHANGED TO XXX'                            
         MVC   P+88(3),C+14                                                     
*                                                                               
STAPRT   GOTO1 =V(PRINTER)                                                      
         OC    FUL,FUL                                                          
         BNZ   NEXT                                                             
         CLC   C+4(3),=C'ALL'                                                   
         BE    STALOOP                                                          
         B     NEXT                                                             
*                                                                               
STAIND   EQU   *                                                                
         GOTO1 =V(DATAMGR),DMCB,(X'00',C+14),PRTQID,X,Q,BUFF                    
         CLI   DMCB+8,0                                                         
         BNE   STAIND1                                                          
         LA    R8,24                                                            
         GOTO1 =V(HEXOUT),PARM,BUFF,P,(R8),=C'TOG'                              
         MVC   P+70(30),=CL30'STATUS CHANGED TO XXX'                            
         MVC   P+88(3),C+14                                                     
STAIND1  GOTO1 =V(PRINTER)                                                      
         B     NEXT                                                             
         EJECT                                                                  
* READ INDEX AND PRINT OUT SECURE REPORT DATA                                   
*                                                                               
SCR      XC    X,X                 CLEAR USER INDEX                             
*                                                                               
SCRLOOP  XC    FUL,FUL             SET NO ERROR                                 
         GOTO1 =V(DATAMGR),DMCB,(X'08',=C'IND'),PRTQID,X,Q,BUFF                 
         CLI   DMCB+8,0                                                         
         BE    SCRPRT                                                           
         ST    R1,FUL              SET EOF/ERR CONDITION                        
         TM    DMCB+8,X'40'                                                     
         BZ    *+6                                                              
         DC    H'0'                DIE IF DISK ERROR                            
         MVC   P,SPACES                                                         
         TM    DMCB+8,X'80'        END OF INDEX                                 
         BZ    *+14                                                             
         MVC   P(30),=CL30'*** END-OF-INDEX ***'                                
         B     SCRPRTX                                                          
         TM    DMCB+8,X'10'        REC NOT FOUND                                
         BZ    *+14                                                             
         MVC   P(30),=CL30'*** INDEX REC NOT FOUND ***'                         
         B     SCRPRTX                                                          
         DC    H'0'                                                             
*                                                                               
SCRPRT   TM    X+09,X'80'          TEST IF SECURE REPORT                        
         BZ    SCRPRT1                                                          
         MVI   P+48,C'>'                                                        
         GOTO1 =V(DATAMGR),DMCB,(X'08',=C'READ'),PRTQID,X,Q,BUFF                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,=A(BUFF)         R4=A(PQ BUFFER)                              
         USING PQRECD,R4                                                        
         TM    PQSECF1,PQSINONO    TEST IF SECURITY FLAGS ARE VALID             
         BO    SCRPRT1                                                          
         TM    PQSECF1,PQSIPID     TEST IF PID PROTECTED REPORT                 
         BZ    SCRPRT1                                                          
         MVI   P+52,C'<'                                                        
SCRPRT1  LA    RF,24                                                            
         GOTO1 =V(HEXOUT),PARM,X+00,P,(RF),=C'TOG'                              
         MVC   P+49(3),=C'XXX'                                                  
         GOTO1 =V(HEXOUT),PARM,X+27,P+53,3,=C'TOG'                              
         CLI   P+48,C'>'                                                        
         BNE   SCRPRTX                                                          
         GOTO1 =V(HEXOUT),PARM,PQSECINF,P+60,6,=C'TOG'                          
*                                                                               
SCRPRTX  GOTO1 =V(PRINTER)                                                      
         CLC   LASTREPT(7),INREPT  EXACT MATCH ON INPUT REPORT                  
         BE    NEXT                                                             
         OC    FUL,FUL                                                          
         BZ    SCRLOOP                                                          
         B     NEXT                                                             
         EJECT                                                                  
* ROUTINE TO TEST TST PQ FUNCTION                                               
*                                                                               
TST      MVC   REPORTID(2),USERID  SET DDS USER ID                              
         MVC   REPORTID+2(3),C+4   SET REPORT ID                                
         MVC   DUB(5),C+7                                                       
         OC    DUB(5),ZEROS                                                     
         PACK  DUB1,DUB(5)                                                      
         CVB   R0,DUB1                                                          
         STCM  R0,3,REPORTID+5     SET REPORT NUMBER                            
         XC    BUFF(40),BUFF                                                    
*                                                                               
         LA    RF,X                READ FIRST CI REC USING REPORT ID            
         USING UKRECD,RF                                                        
         MVC   UKKEY(7),REPORTID                                                
         MVI   UKFLAG,UKFLNUM+UKFLCIA+UKFLCIR                                   
         GOTO1 =V(DATAMGR),DMCB,INDEX,PRTQUE,X,Q,BUFF                           
         CLI   8(R1),0                                                          
         BNE   TSTERR                                                           
         LA    RF,X                EXTRACT RETURN VALUES                        
         MVC   DSKADR,UKUSRINF                                                  
         MVC   PRTQID,PRTQUE                                                    
         MVC   PRTQID+4(1),UKUSRINF+4                                           
         DROP  RF                                                               
         GOTO1 =V(HEXOUT),PARM,X+00,P+00,40,=C'TOG'                             
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(HEXOUT),PARM,BUFF,P+00,40,=C'TOG'                             
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         OI    BUFF+(PQATTB-PQINDEX),PQATUSR+PQATJOBO                           
         MVI   BUFF+(PQREPTY-PQINDEX),C'$'                                      
         GOTO1 =V(DATAMGR),DMCB,DMWRT,PRTQID,DSKADR,BUFF                        
         CLI   8(R1),0                                                          
         BNE   TSTERR                                                           
         GOTO1 =V(HEXOUT),PARM,X+00,P+00,40,=C'TOG'                             
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(HEXOUT),PARM,BUFF,P+00,40,=C'TOG'                             
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    RF,X                TURN ON JOBO FLAG IN INDEX ENTRY             
         USING UKRECD,RF                                                        
         XC    X,X                                                              
         MVC   UKKEY(7),REPORTID                                                
         MVI   UKFLAG,UKFLNUM                                                   
         GOTO1 =V(DATAMGR),DMCB,INDEX,PRTQID,X,Q,BUFF                           
         CLI   8(R1),0                                                          
         BNE   TSTERR                                                           
         GOTO1 =V(HEXOUT),PARM,X+00,P+00,40,=C'TOG'                             
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(HEXOUT),PARM,BUFF,P+00,40,=C'TOG'                             
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,JOSET,PRTQID,X,Q,BUFF                           
         CLI   8(R1),0                                                          
         BNE   TSTERR                                                           
         GOTO1 =V(HEXOUT),PARM,X+00,P+00,40,=C'TOG'                             
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(HEXOUT),PARM,BUFF,P+00,40,=C'TOG'                             
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         DROP  RF                                                               
         B     TSTEXIT                                                          
TSTERR   DC    H'0'                                                             
TSTEXIT  B     NEXT                                                             
         DC    CL8'*TSTTST*'                                                    
REPORTID DC    CL8'        '                                                    
PRTQUE   DC    CL8'PRTQU   '                                                    
INDEX    DC    CL8'INDEX   '                                                    
DMWRT    DC    CL8'DMWRT   '                                                    
JOSET    DC    CL8'JOSET   '                                                    
         EJECT                                                                  
* NOP    ORG   PQTEST+(((*-PQTEST)/4096)+1)*4096                                
* STANDARD DMPRTQR ROUTINES AND WORKING STARAGE. MUST START AT >4K              
* DMPRTQR                                                                       
       ++INCLUDE DMPRTQR                                                        
         EJECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
DUB2     DS    D                                                                
FUL      DS    F                                                                
FULL     DS    F                                                                
OCISZ#1  DS    H                                                                
OCISZ#2  DS    H                                                                
NCISZ#1  DS    H                                                                
NCISZ#2  DS    H                                                                
*                                                                               
FRSTCI   DS    F                                                                
LASTCI   DS    F                                                                
LASTTK   DS    F                                                                
DSKADR   DS    F                                                                
NUMTRKS  DS    H                                                                
*                                                                               
OPART#2  DS    H                                                                
NPART#2  DS    H                                                                
OTRKSW   DS    H                                                                
NTRKSW   DS    H                                                                
*                                                                               
TTREPS   DS    F                                                                
TTSGLS   DS    F                                                                
TTDBLS   DS    F                                                                
TTBADS   DS    F                                                                
TTMIXS   DS    F                                                                
TTTRKS   DS    F                                                                
TOTRKSW  DS    F                                                                
TOPART#2 DS    F                                                                
TNTRKSW  DS    F                                                                
TNPART#2 DS    F                                                                
*                                                                               
TCIL#1   DS    F                                                                
TCIL#2   DS    F                                                                
TCID#1   DS    F                                                                
TCID#2   DS    F                                                                
CIATABN  DS    F                                                                
CIATABP  DS    F                                                                
CIATABL  DS    F                                                                
CIATABT  DS    F                                                                
CIATABA  DS    A                                                                
PURTABN  DS    F                                                                
PURTABA  DS    A                                                                
P1       DS    6F                                                               
DMCB     DS    6F                                                               
DMCB1    DS    6F                                                               
PARM     DS    6F                                                               
PRTQID   DC    CL8'PRTQU'                                                       
PRTQ1    DC    CL8'PRTQ1'                                                       
LASTREPT DC    XL8'00'                                                          
INREPT   DC    XL8'00'                                                          
ZEROS    DC    CL6'000000'                                                      
WORK     DS    CL18                                                             
         DS    0D                                                               
         DC    C'*KFWKFW*'                                                      
*DMPRTQW                                                                        
       ++INCLUDE DMPRTQW                                                        
         DC    XL24'00'                                                         
*                                                                               
LINES    DS    H                                                                
PAGES    DS    H                                                                
LINNO    DS    PL4                                                              
PAGNO    DS    PL4                                                              
*                                                                               
C        DS    CL80                                                             
*                                                                               
SOFLAB   DC    X'00',C'SOF',X'00'                                               
EOFLAB   DC    X'FF',C'EOF',X'FF'                                               
OSOFLAB  DC    X'0000',C'SOFSOF',X'0000'                                        
OEOFLAB  DC    X'FFFF',C'EOFEOF',X'FFFF'                                        
*                                                                               
PFLAG    DC    C'Y'                SET BY PRINT=Y/N/E CARD                      
WFLAG    DC    C' '                SET BY WRITE=Y/N CARD                        
UPDATE   DC    C' '                VALUE USED TO VARY ACTION                    
TYPEPQ   DC    C'O'                NEW=N OLD=O (NEW MUST INCLUDE DN...)         
THISPQ   DC    C' '                SET BY DMPRTQ BUFF CALL                      
USERID   DC    XL2'00'                                                          
*&&UK                                                                           
U1       DC    H'33'               DDS2...ODD                                   
UO       DC    H'33'                                                            
U2       DC    H'38'               DDS1...EVEN                                  
UE       DC    H'38'                                                            
*&&                                                                             
*&&US                                                                           
U1       DC    H'43'               TCH1...ODD                                   
UO       DC    H'43'                                                            
U2       DC    H'236'              TCH2...EVEN                                  
UE       DC    H'236'                                                           
*&&                                                                             
SAVEI    DC    CL198' '                                                         
SAVEP    DC    CL198' '                                                         
                                                                                
         LTORG                                                                  
                                                                                
         DS    0D                                                               
         DC    C'*NDXNDX*'                                                      
X        DC    XL56'00'                                                         
                                                                                
         DS    0D                                                               
         DC    C'*LNELNE*'                                                      
QH       DC    XL4'00'                                                          
Q        DC    CL166' '                                                         
                                                                                
         DS    0D                                                               
         DC    C'*SOBSOB*'                                                      
CXREC    DS    0C                                                               
BUFF     DC    14336X'00'                                                       
         DC    C'*EOBEOB*'                                                      
BUFF1    DC    14336X'00'                                                       
         EJECT                                                                  
* ROUTINE TO CHECK DATAMGR RETURN CODE AND OUTPUT MESSAGE                       
*                                                                               
CHKERR   NTR1  BASE=*                                                           
         CLI   8(R1),0                                                          
         BE    CHKERRX                                                          
         L     RF,=A(PQERRS)                                                    
CHKERR2  CLI   0(RF),0             TEST END OF TABLE - UNKNOWN ERROR            
         BE    CHKERR3                                                          
         CLC   0(1,RF),8(R1)                                                    
         BE    CHKERR3                                                          
         LA    RF,L'PQERRS(RF)                                                  
         B     CHKERR2                                                          
CHKERR3  MVC   CHKERRM1(3),0(RF)   DMCB+8/ABEND CODE                            
         MVC   CHKERRM4(25),3(RF)  TEXT                                         
CHKERR4  L     RF,8(RD)            LOOK FOR DMPRTQ'S WORKING STORAGE            
         CLC   0(4,RF),=C'PRTQ'                                                 
         BE    CHKERR4A                                                         
         L     RF,8(RF)                                                         
         CLC   0(4,RF),=C'PRTQ'                                                 
         BNE   CHKERR5                                                          
CHKERR4A LA    RF,72(RF)                                                        
         CLC   0(8,RF),=C'PQWSINFO'                                             
         BNE   CHKERR5                                                          
         CLC   12(4,RF),=C'PRTQ'   TEST IF USERPRTQ NAME FILLED IN              
         BNE   CHKERR5                                                          
         MVC   CHKERRM4+4(1),16(RF)                                             
         LA    RF,24(RF)           POINT TO REPTINFO                            
         OC    0(2,RF),0(RF)                                                    
         BZ    CHKERR5                                                          
         MVC   CHKERRM5(3),2(RF)   REPORT ID                                    
         MVI   CHKERRM5+3,C','                                                  
         SR    R0,R0                                                            
         ICM   R0,3,5(RF)                                                       
         CVD   R0,CHKERRDW                                                      
         OI    CHKERRDW+7,X'0F'                                                 
         UNPK  CHKERRM5+4(5),CHKERRDW                                           
CHKERR5  LA    R3,CHKERRM3         POINT TO MESSAGE LENGTH                      
         WTO   TEXT=(R3)                                                        
         LA    R1,DMCB                                                          
         SR    R0,R0                                                            
         IC    R0,8(R1)            R0=DMCB+8                                    
         STC   R0,CHKERRM1                                                      
CHKERR6  LA    R1,DMCB                                                          
         SR    R0,R0                                                            
         IC    R0,CHKERRM1         R0=DMCB+8                                    
         LH    R3,CHKERRM2         R3=ABEND CODE                                
         ABEND (3)                                                              
CHKERRX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
CHKERRDW DC    D'0'                                                             
CHKERRRE DC    F'0'                                                             
CHKERRMR DC    H'0'                                                             
         DC    X'00'               ALIGN                                        
CHKERRM1 DC    X'00'               DMCB+8                                       
CHKERRM2 DC    H'0'                ABEND CODE                                   
CHKERRM3 DC    H'36'               WTO MSG LENGTH                               
CHKERRM4 DC    CL26' '             WTO MSG TEXT                                 
CHKERRM5 DC    CL10' '             WTO MSG TEXT                                 
CHKWRK   DC    CL16' '                                                          
         DS    0F                                                               
PQERRS   DS    0XL28                                                            
         DC    X'80',AL2(130),CL25'PRTQ--EOF'                                   
         DC    X'81',AL2(131),CL25'PRTQ--EOF NO PART#1 CIS'                     
         DC    X'82',AL2(132),CL25'PRTQ--EOF NO PART#2 CIS'                     
         DC    X'84',AL2(133),CL25'PRTQ--REPORT TOO BIG'                        
         DC    X'88',AL2(134),CL25'PRTQ--USERID MAX REPORTS'                    
         DC    X'40',AL2(135),CL25'PRTQ--DISK ERROR'                            
         DC    X'41',AL2(136),CL25'PRTQ--FORMAT ERR ON OPEN'                    
         DC    X'42',AL2(137),CL25'PRTQ--FORMAT ERR ON CLOSE'                   
PQERRSX  DC    X'00',AL2(138),CL25'PRTQ--UNKNOWN ERROR'                         
         EJECT                                                                  
* CARD FORMAT  CHK SSSNNNNNC WRITE=Y                                            
*                                                                               
CHK      NTR1  BASE=*                                                           
         L     R4,=A(BUFF)         INIT BUFFER - MUST KNOW PRTQID               
         USING PQRECD,R4                                                        
         GOTO1 =V(DATAMGR),DMCB,=C'BUFF',PRTQID,X,Q,(R4)                        
         LA    RE,12(R4)           RE=A(CIDATA)                                 
         CLI   16(RE),0                                                         
         BNE   CHK0A                                                            
         MVC   CIDATA(64),0(RE)    MOVE NEW PQ DATA                             
         MVI   THISPQ,C'N'                                                      
         B     CHK0B                                                            
CHK0A    MVC   CIDATA(40),0(RE)    MOVE OLD PQ DATA                             
         MVC   CINDXLN,=H'24'                                                   
         MVI   THISPQ,C'O'                                                      
CHK0B    L     RF,=V(DMENQDEQ)     NEEDED TO CALL LOCKING ROUTINE               
         ST    RF,CIENQDEQ                                                      
*                                                                               
CHK1     XC    X,X                 CLEAR USER INDEX                             
         CLC   C+4(3),SPACES                                                    
         BE    CHK1A                                                            
         MVC   X(2),USERID         SET DDS USER ID                              
         MVC   X+2(3),C+4          SET REPORT ID                                
         MVC   DUB(5),C+7                                                       
         OC    DUB(5),ZEROS                                                     
         PACK  DUB1,DUB(5)                                                      
         CVB   R0,DUB1                                                          
         LTR   R0,R0                                                            
         BZ    CHK1A                                                            
         STCM  R0,3,X+5            SET REPORT NUMBER                            
         CLI   C+12,C' '                                                        
         BE    CHK1A                                                            
         MVC   X+7(1),C+12         SET REPORT CLASS                             
*                                                                               
CHK1A    MVC   UPDATE,WFLAG        SET TO VALUE IN WRITE=Y/N CARD               
         CLC   C+14(6),=C'WRITE='                                               
         BNE   *+10                                                             
         MVC   UPDATE,C+20         CAN BE MODIFIED ON THIS CARD                 
         CLI   UPDATE,C' '                                                      
         BNE   *+8                                                              
         MVI   UPDATE,C'N'         THIS DEFAULTS TO NON UPDATIVE                
*                                                                               
CHK1B    XC    TCIL#1,TCIL#1       CLEAR TOTAL COUNTERS                         
         XC    TCIL#2,TCIL#2                                                    
         XC    TCID#1,TCID#1                                                    
         XC    TCID#2,TCID#2                                                    
         XC    TTMIXS,TTMIXS                                                    
         XC    TTBADS,TTBADS                                                    
         L     R0,=A(CIATAB)       CLEAR CI ADDR TABLE                          
         ST    R0,CIATABA                                                       
         XC    CIATABN,CIATABN                                                  
         XC    CIATABP,CIATABP                                                  
         XC    CIATABL,CIATABL                                                  
         XC    CIATABT,CIATABT                                                  
         L     R1,=A(CIATABX)                                                   
         SR    R1,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
CHK1C    L     R0,=A(PURTAB)       CLEAR PURGE CI TABLE                         
         ST    R0,PURTABA                                                       
         XC    PURTABN,PURTABN                                                  
         L     R1,=A(PURTABX)                                                   
         SR    R1,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
CHK1D    MVC   P(30),=CL30'*** STR-OF-PART#1 INDEX ***'                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
CHKLOOP  XC    FUL,FUL                                                          
         GOTO1 =V(DATAMGR),DMCB,(X'08',=C'IND'),PRTQID,X,Q,BUFF                 
         CLI   DMCB+8,0                                                         
         BE    CHKRCI                                                           
         ST    R1,FUL              SET EOF/ERR CONDITION                        
         TM    DMCB+8,X'40'                                                     
         BZ    *+6                                                              
         DC    H'0'                DIE IF DISK ERROR                            
         MVC   P,SPACES                                                         
         TM    DMCB+8,X'80'        END OF INDEX                                 
         BO    CHKPRTA                                                          
         TM    DMCB+8,X'10'        REC NOT FOUND                                
         BZ    *+14                                                             
         MVC   P(30),=CL30'*** INDEX REC NOT FOUND ***'                         
         B     CHKPRTA                                                          
         DC    H'0'                                                             
*                                                                               
CHKRCI   ICM   R0,7,X+27           R0=..0TTTTT                                  
         SLL   R0,12                                                            
         ST    R0,DSKADR                                                        
         MVI   DSKADR+3,1          DSKADR=TTTTT001                              
         CLI   THISPQ,C'N'                                                      
         BE    CHKRCI1                                                          
         MVC   DSKADR(2),X+28                                                   
         MVC   DSKADR+2(2),=X'0100'                                             
         DROP  R4                                                               
CHKRCI1  L     R5,=A(BUFF1)        R5=A(FIRST CI REC IN BUFF1)                  
         USING PQRECD,R5                                                        
         GOTO1 =V(DATAMGR),DMCB1,=C'DMREAD',PRTQID,DSKADR,(R5)                  
         CLI   8(R1),0                                                          
         BNE   CHKBAD                                                           
CHKRCI2  SR    RE,RE               RE=NUM OF PART#1 CIS                         
         SR    RF,RF               RF=NUM OF PART#2 CIS                         
         TM    PQTYPE,PQTYNEW                                                   
         BO    CHKRCI3                                                          
         CLI   THISPQ,C'O'         TEST REPORT TYPE WITH PQ TYPE                
         BE    CHKRCI2A                                                         
         L     R0,TTMIXS           BUMP MIS MATCH TYPE COUNT                    
         AHI   R0,1                                                             
         ST    R0,TTMIXS                                                        
         MVI   P+48,C'>'                                                        
         MVI   P+52,C'1'           REPORT TYPE NOT EQUAL TO FILE TYPE           
         B     CHKRCI3                                                          
CHKRCI2A IC    RE,OQNCI            OLD PQ VALUES                                
         CHI   RE,1                                                             
         BE    CHKRCI4                                                          
         IC    RF,OQNCIX                                                        
         AR    RF,RE                                                            
         AHI   RF,-1                                                            
         LA    RE,1                                                             
         B     CHKRCI4                                                          
CHKRCI3  CLI   THISPQ,C'N'         TEST REPORT TYPE WITH PQ TYPE                
         BE    CHKRCI3A                                                         
         L     R0,TTMIXS           BUMP MIS MATCH TYPE COUNT                    
         AHI   R0,1                                                             
         ST    R0,TTMIXS                                                        
         MVI   P+48,C'>'                                                        
         MVI   P+52,C'1'           REPORT TYPE NOT EQUAL TO FILE TYPE           
         B     CHKRCI2A                                                         
CHKRCI3A IC    RE,PQNCI            NEW PQ VALUES                                
         CHI   RE,1                                                             
         BE    CHKRCI4                                                          
         IC    RF,PQNCIX                                                        
         AR    RF,RE                                                            
         AHI   RF,-1                                                            
         LA    RE,1                                                             
CHKRCI4  TM    X+10,X'C0'          TEST LIVE INDEX ENTRY                        
         BZ    CHKRCI5                                                          
         A     RE,TCIL#1           BUMP LIVE PART#1 CI COUNT                    
         ST    RE,TCIL#1                                                        
         A     RF,TCIL#2           BUMP LIVE PART#2 CI COUNT                    
         ST    RF,TCIL#2                                                        
         B     CHKRCI6                                                          
CHKRCI5  A     RE,TCID#1           BUMP DEAD PART#1 CI COUNT                    
         ST    RE,TCID#1                                                        
         A     RF,TCID#2           BUMP DEAD PART#2 CI COUNT                    
         ST    RF,TCID#2                                                        
CHKRCI6  B     CHKPRT                                                           
*                                                                               
CHKBAD   L     RF,TTBADS           BUMP BAD REPORT COUNTS                       
         AHI   RF,1                                                             
         ST    RF,TTBADS                                                        
         GOTO1 =V(HEXOUT),PARM,X,P,24,=C'TOG'                                   
         MVC   P+48(5),=C'>NDX3'   PRTQ FILE READ ERROR                         
         GOTO1 =V(HEXOUT),PARM,X+27,P+53,3,=C'TOG'                              
         MVC   P+60(7),=C'DSKADR='                                              
         GOTO1 =V(HEXOUT),PARM,DSKADR,P+67,4,=C'TOG'                            
         CLI   PFLAG,C'N'                                                       
         BE    CHKLOOP                                                          
         GOTO1 =V(PRINTER)                                                      
         B     CHKLOOP             BACK FOR NEXT INDEX ENTRY                    
*                                                                               
CHKPRT   GOTO1 =V(HEXOUT),PARM,X,P,24,=C'TOG'                                   
         MVC   P+49(3),=C'NDX'                                                  
         GOTO1 =V(HEXOUT),PARM,X+27,P+53,3,=C'TOG'                              
         MVC   SAVEI,P                                                          
         CLI   PFLAG,C'Y'                                                       
         BNE   *+10                                                             
         GOTO1 =V(PRINTER)         PRINT INDEX ENTRY                            
         MVC   P,SPACES                                                         
         B     CHKPRT2                                                          
*                                                                               
CHKPRT1  GOTO1 =V(DATAMGR),DMCB1,=C'DMREAD',PRTQID,DSKADR,(R5)                  
         CLI   8(R1),0                                                          
         BNE   CHKBAD                                                           
*                                                                               
CHKPRT2  GOTO1 =V(HEXOUT),PARM,(R5),P,24,=C'TOG'                                
         SR    R0,R0                                                            
         IC    R0,PQSEQ                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+49(3),DUB+6(2)                                                 
         GOTO1 =V(HEXOUT),PARM,PQBATTR,P+53,16,=C'TOG'                          
         CLI   PQSEQ,1             GET FIRST PART OF CI DATA                    
         BH    CHKPRT2A                                                         
         LA    RF,PQDATA                                                        
         CLI   THISPQ,C'N'                                                      
         BE    *+8                                                              
         LA    RF,OQDATA                                                        
         GOTO1 =V(HEXOUT),PARM,(RF),P+86,18,=C'TOG'                             
*                                                                               
CHKPRT2A MVC   SAVEP,P             SAVE THIS PRINT LINE                         
         CLC   X(7),0(R5)                                                       
         BE    CHKPRT2B                                                         
         MVI   P+48,C'>'                                                        
         MVI   P+52,C'2'           INDEX KEY NEQ CI KEY                         
CHKPRT2B CLI   PFLAG,C'N'                                                       
         BE    CHKPRT2N            PRINT=N                                      
         BH    CHKPRT2D            PRINT=Y                                      
         CLI   P+48,C'>'           PRINT=E PRINT IF ERROR                       
         BNE   CHKPRT2N                                                         
         CLI   SAVEI,C' '          TEST IF PRINTED SAVED INDEX ENTRY            
         BE    CHKPRT2C                                                         
         MVC   P,SAVEI                                                          
         GOTO1 =V(PRINTER)                                                      
         MVC   SAVEI,SPACES                                                     
CHKPRT2C MVC   P,SAVEP                                                          
CHKPRT2D GOTO1 =V(PRINTER)         PRINT CI DATA                                
CHKPRT2N MVC   P,SPACES                                                         
*                                                                               
CHKPRT3  CLI   THISPQ,C'N'         GET NEXT CI DISK ADDR                        
         BE    CHKPRT3A                                                         
         OC    OQCINEXT,OQCINEXT                                                
         BZ    CHKPRT4                                                          
         MVC   DSKADR(2),OQCINEXT                                               
         MVC   DSKADR+2(2),=X'0100'                                             
         B     CHKPRT3B                                                         
CHKPRT3A OC    PQCINEXT,PQCINEXT                                                
         BZ    CHKPRT4                                                          
         MVC   DSKADR,PQCINEXT                                                  
CHKPRT3B L     RE,CIATABA                                                       
         CLC   0(4,RE),=4X'FF'     TEST IF SPACE IN TABLE                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,RE),DSKADR      MOVE PART2 CI ADDR TO TABLE                  
         LA    RE,4(RE)                                                         
         ST    RE,CIATABA                                                       
         L     RE,CIATABN          BUMP NUMBER OF TABLE ENTRIES                 
         LA    RE,1(RE)                                                         
         ST    RE,CIATABN                                                       
         B     CHKPRT1                                                          
CHKPRT4  OC    FUL,FUL                                                          
         BZ    CHKLOOP                                                          
*                                                                               
CHKPRTA  MVC   P(30),=CL30'*** END-OF-PART#1 INDEX ***'                         
         GOTO1 =V(PRINTER)                                                      
         OC    FUL,FUL                                                          
         BZ    CHKLOOP                                                          
         CLC   C+4(3),SPACES       TEST IF ANY FILTERS SPECIFIED                
         BNE   CHKT                                                             
*                                                                               
CHKPRTB  XC    DMCB(24),DMCB       SORT LIST OF PART2 CI ADDRS                  
         L     R6,=A(CIATAB)                                                    
         ST    R6,DMCB             R6=A(FIRST ENTRY)                            
         ICM   R0,15,CIATABN                                                    
         BZ    CHKT                                                             
         ST    R0,DMCB+4           R0=NUM OF ENTRIES                            
         MVC   DMCB+8(4),=F'4'     LEN OF RECORD                                
         MVC   DMCB+12(4),=F'4'    LEN OF KEY                                   
         GOTO1 =V(QSORT),DMCB                                                   
*                                                                               
CHK2     CLC   THISPQ,TYPEPQ       CAN ONLY CHK PART2 INDEX IF MATCH            
         BNE   CHKT                                                             
         GOTO1 =V(PRINTER)                                                      
         MVC   P(30),=CL30'*** STR-OF-PART#2 INDEX ***'                         
         GOTO1 =V(PRINTER)                                                      
         BAS   RE,CXLOOPJ          POINT TO START OF PART2 INDEX                
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
         CLC   CIADDR,0(R6)        IS CIADDR NEXT IN TABLE                      
         BNE   CHK2D               NO                                           
         CLI   PQSTAT,PQSTPU       YES CANT BE PURGED                           
         BE    CHK2C                                                            
         LA    R6,4(R6)            THIS IS OK SO BUMP TO NEXT                   
         B     CHK2N                                                            
*                                                                               
CHK2C    MVC   P+48(5),=C'>NDX4'   ERROR PURGED PART2 IN TABLE                  
         LA    RF,24                                                            
         GOTO1 =V(HEXOUT),PARM,(R5),P,(RF),=C'TOG'                              
         GOTO1 =V(HEXOUT),PARM,CIADDR,P+53,4,=C'TOG'                            
         CLI   PFLAG,C'N'                                                       
         BE    *+10                                                             
         GOTO1 =V(PRINTER)                                                      
         LA    R6,4(R6)                                                         
         B     CHK2N                                                            
*                                                                               
CHK2D    CLI   PQSTAT,PQSTPU       CIADDR NOT IN TABLE                          
         BNE   CHK2E               THIS IS OK IF ITS PURGED                     
         L     R1,CIATABP                                                       
         LA    R1,1(R1)                                                         
         ST    R1,CIATABP          BUMP PURGED PART2 CIS                        
         B     CHK2N                                                            
*                                                                               
CHK2E    MVC   P+48(5),=C'>NDX5'   ERROR NON PURGED PART2 NOT IN TAB            
         TM    PQSTAT,PQSTTE                                                    
         BZ    CHK2E1                                                           
         CLI   PQAGERT,X'FF'                                                    
         BNE   CHK2E1                                                           
         MVC   P+48(5),=C'>NDX6'   ERROR TEMPORARY PART2 NOT IN TAB             
         B     CHK2F                                                            
*                                                                               
CHK2E1   L     RE,PURTABA          TEST IF SPACE IN PURGE CI TABLE              
         CLC   0(4,RE),=4X'FF'                                                  
         BE    CHK2F                                                            
         MVC   0(4,RE),CXADDR      MOVE INDEX DSKADR TO TABLE                   
         MVC   4(2,RE),CXPAGE      MOVE INDEX PAGE/ENTRY TO TABLE               
         MVC   6(2,RE),CXENTRY                                                  
         MVC   8(4,RE),CIADDR      MOVE CI DSKADR TO TABLE                      
         LA    RE,12(RE)                                                        
         ST    RE,PURTABA                                                       
         L     RE,PURTABN          BUMP NUMBER OF PURGE TABLE ENTRYS            
         LA    RE,1(RE)                                                         
         ST    RE,PURTABN                                                       
*                                                                               
CHK2F    LA    RF,24                                                            
         GOTO1 =V(HEXOUT),PARM,(R5),P,(RF),=C'TOG'                              
         GOTO1 =V(HEXOUT),PARM,CIADDR,P+53,4,=C'TOG'                            
         CLI   PFLAG,C'N'                                                       
         BE    *+10                                                             
         GOTO1 =V(PRINTER)                                                      
         TM    PQSTAT,PQSTTE                                                    
         BZ    CHK2G                                                            
         CLI   PQAGERT,X'FF'                                                    
         BNE   CHK2G                                                            
         L     R1,CIATABT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,CIATABT          BUMP TEMP PART2 CIS                          
         B     CHK2N                                                            
CHK2G    L     R1,CIATABL                                                       
         LA    R1,1(R1)                                                         
         ST    R1,CIATABL          BUMP LOST PART2 CIS                          
         B     CHK2N                                                            
*                                                                               
CHK2N    BAS   RE,CXLOOPX          BUMP TO NEXT INDEX ENTRY                     
         B     CHK2B                                                            
         B     CHK2A               END OF PAGE                                  
*                                                                               
         MVC   P(30),=CL30'*** END-OF-PART#2 INDEX ***'                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
CHKPUR   ICM   R7,15,PURTABN       TEST IF ANY ENTRIES IN PURGE TABLE           
         BZ    CHKT                                                             
         CLI   UPDATE,C'Y'         TEST IF WRITE=Y SPECIFIED                    
         BNE   CHKT                                                             
         GOTO1 =V(PRINTER)                                                      
         MVC   P(30),=CL30'*** LOST PART#2 PURGED ***'                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   UPDATE,C'Y'                                                      
         BNE   *+8                                                              
         BAS   RE,PQLOCK           LOCK PRTQ FILE                               
*                                                                               
         BAS   RE,CXLOOPJ          POINT TO START OF PART2 INDEX                
         USING PQRECD,R5           R5=A(WRKF INDEX ENTRY)                       
         L     R6,=A(PURTAB)       R6=A(PURGE TABLE)                            
*                                                                               
CHKPURA  BAS   RE,GETXAD           GET DSKADR OF NEXT INDEX PAGE                
         CLC   CXADDR,0(R6)                                                     
         BNE   CHKPURN                                                          
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),PRTQID,CXADDR,CXREC              
         CLI   8(R1),0                                                          
         BE    CHKPURB                                                          
         CLI   UPDATE,C'Y'                                                      
         BNE   *+8                                                              
         BAS   RE,PQUNLK           UNLOCK PRTQ FILE                             
         DC    H'0'                                                             
*                                                                               
CHKPURB  CLC   CXADDR,0(R6)        TEST IF INDEX PAGE IN PURGE TABLE            
         BNE   CHKPURN                                                          
         CLC   CXPAGE,4(R6)        TEST IF INDEX ENTRY IN PURGE TABLE           
         BNE   CHKPURN                                                          
         CLC   CXENTRY,6(R6)                                                    
         BNE   CHKPURN                                                          
         MVC   X,PQINDEX           SAVE INDEX ENTRY                             
         LH    R1,CINDXLN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    PQINDEX(0),PQINDEX  CLEAR INDEX ENTRY                            
         LA    RF,24                                                            
         GOTO1 =V(HEXOUT),PARM,X,P,(RF),=C'TOG'                                 
         GOTO1 =V(HEXOUT),PARM,8(R6),P+53,4,=C'TOG'                             
         GOTO1 =V(HEXOUT),PARM,0(R6),P+62,4,=C'TOG'                             
         GOTO1 =V(HEXOUT),PARM,4(R6),P+71,4,=C'TOG'                             
         MVI   P+48,C'>'                                                        
         MVC   P+49(3),=C'PUR'                                                  
         LA    R6,12(R6)           BUMP TO NEXT PURGE TABLE ENTRY               
         BCTR  R7,0                                                             
         CLC   CXADDR,0(R6)        TEST IF STILL IN THIS PAGE                   
         BNE   CHKPURC                                                          
         GOTO1 =V(PRINTER)                                                      
         B     CHKPURN                                                          
*                                                                               
CHKPURC  MVI   P+52,C'>'           SET INDEX PAGE WRITTEN FLAG                  
         GOTO1 =V(PRINTER)                                                      
         CLI   UPDATE,C'Y'                                                      
         BNE   CHKPURN                                                          
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMWRT'),PRTQID,CXADDR,CXREC               
         CLI   8(R1),0                                                          
         BE    CHKPURN                                                          
         BAS   RE,PQUNLK           UNLOCK PRTQ FILE                             
         DC    H'0'                                                             
*                                                                               
CHKPURN  LTR   R7,R7               TEST END OF PURGE TABLE                      
         BZ    CHKPURX                                                          
         BAS   RE,CXLOOPX          BUMP TO NEXT INDEX ENTRY                     
         B     CHKPURB                                                          
         B     CHKPURA             END OF PAGE                                  
*                                                                               
CHKPURX  CLI   UPDATE,C'Y'                                                      
         BNE   CHKT                                                             
         BAS   RE,PQUNLK           UNLOCK PRTQ FILE                             
*                                                                               
*                                                                               
CHKT     GOTO1 =V(PRINTER)                                                      
         MVC   P(15),=CL15'PRTQX CI TOTALS'                                     
         MVC   P(5),PRTQID                                                      
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
         LA    R7,P                                                             
*                                                                               
         MVC   0(7,R7),=C'TOTL#1='                                              
         SR    R0,R0                                                            
         ICM   R0,3,CICITOT                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(5,R7),DUB                                                      
         LA    R7,13(R7)                                                        
*                                                                               
         MVC   0(7,R7),=C'LIVE#1='                                              
         L     R0,TCIL#1                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(5,R7),DUB                                                      
         LA    R7,13(R7)                                                        
*                                                                               
         MVC   0(7,R7),=C'DEAD#1='                                              
         L     R0,TCID#1                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(5,R7),DUB                                                      
         LA    R7,13(R7)                                                        
*                                                                               
         ST    R7,FULL             SAVE WHERE PART#2 DATA STARTS                
*                                                                               
         MVC   0(7,R7),=C'TOTL#2='                                              
         SR    R0,R0                                                            
         ICM   R0,3,CJCITOT                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(5,R7),DUB                                                      
         LA    R7,13(R7)                                                        
*                                                                               
         MVC   0(7,R7),=C'LIVE#2='                                              
         L     R0,TCIL#2                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(5,R7),DUB                                                      
         LA    R7,13(R7)                                                        
*                                                                               
         MVC   0(7,R7),=C'DEAD#2='                                              
         L     R0,TCID#2                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(5,R7),DUB                                                      
         LA    R7,13(R7)                                                        
*                                                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
CHKT2    CLC   THISPQ,TYPEPQ       NO PART#2 COUNTERS IF MIS MATCH              
         BNE   CHKX                                                             
         L     R7,FULL             POSITION TO PART#2 DATA                      
*                                                                               
         MVC   0(7,R7),=C'PRGD#2='                                              
         L     R0,CIATABP                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(5,R7),DUB                                                      
         LA    R7,13(R7)                                                        
*                                                                               
         MVC   0(7,R7),=C'LOST#2='                                              
         L     R0,CIATABL                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(5,R7),DUB                                                      
         LA    R7,13(R7)                                                        
*                                                                               
         MVC   0(7,R7),=C'TEMP#2='                                              
         L     R0,CIATABT                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(5,R7),DUB                                                      
         LA    R7,13(R7)                                                        
*                                                                               
CHKTX    GOTO1 =V(PRINTER)                                                      
*                                                                               
CHKX     XIT1                                                                   
         DROP  R5                                                               
         USING PQRECD,R4                                                        
         LTORG                                                                  
         EJECT                                                                  
* CARD FORMAT  ANA II JJ - II IS NEW PART#1 SIZE AND JJ IS PART#2 SIZE          
*                                                                               
ANA      NTR1  BASE=*                                                           
         L     R4,=A(BUFF)         INIT BUFFER - MUST KNOW PRTQID               
         USING PQRECD,R4                                                        
         GOTO1 =V(DATAMGR),DMCB,=C'BUFF',PRTQID,X,Q,(R4)                        
         LA    RE,12(R4)           RE=A(CIDATA)                                 
         CLI   16(RE),0                                                         
         BNE   *+18                                                             
         MVC   CIDATA(64),0(RE)    MOVE NEW PQ DATA                             
         MVI   THISPQ,C'N'                                                      
         B     ANA0                                                             
         MVC   CIDATA(40),0(RE)    MOVE OLD PQ DATA                             
         MVC   CINDXLN,=H'24'                                                   
         MVI   THISPQ,C'O'                                                      
*                                                                               
ANA0     MVC   OCISZ#1,CITRKS      SET OLD/NEW PART#1/PART#2 SIZES              
         MVC   NCISZ#1,CITRKS                                                   
         MVC   OCISZ#2,CJTRKS                                                   
         MVC   NCISZ#2,CJTRKS                                                   
         CLC   THISPQ,TYPEPQ       CAN ONLY ANALIZE MATCHING PQ                 
         BNE   ANAERR                                                           
         CLC   C+4(2),SPACES       TEST IF CI SIZES INPUT                       
         BE    ANA1                                                             
         MVC   DUB(2),C+4                                                       
         OC    DUB(2),ZEROS                                                     
         PACK  DUB1,DUB(2)                                                      
         CVB   R0,DUB1                                                          
         CHI   R0,1                                                             
         BL    ANAERR                                                           
         CHI   R0,5                                                             
         BH    ANAERR                                                           
         STH   R0,NCISZ#1          PART#1 CI SIZE (CITRKS)                      
         MVC   DUB(2),C+7                                                       
         OC    DUB(2),ZEROS                                                     
         PACK  DUB1,DUB(2)                                                      
         CVB   R0,DUB1                                                          
         CHI   R0,1                                                             
         BL    ANAERR                                                           
         CHI   R0,20                                                            
         BH    ANAERR                                                           
         STH   R0,NCISZ#2          PART#2 CI SIZE (CJTRKS)                      
*                                                                               
ANA1     CLI   PFLAG,C'Y'          INIT FOR PRINTING                            
         BNE   ANA2                                                             
         MVC   TITLE+26(5),PRTQID                                               
         MVC   MID1(84),ANAMID1                                                 
         MVC   MID2(84),ANAMID2                                                 
         ZAP   LINE,=P'99'                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
ANA2     XC    TTREPS,TTREPS       CLEAR TOTAL COUNTERS                         
         XC    TTSGLS,TTSGLS                                                    
         XC    TTDBLS,TTDBLS                                                    
         XC    TTBADS,TTBADS                                                    
         XC    TTMIXS,TTMIXS                                                    
         XC    TTTRKS,TTTRKS                                                    
         XC    TOPART#2,TOPART#2                                                
         XC    TOTRKSW,TOTRKSW                                                  
         XC    TNPART#2,TNPART#2                                                
         XC    TNTRKSW,TNTRKSW                                                  
         XC    TCIL#1,TCIL#1                                                    
         XC    TCIL#2,TCIL#2                                                    
         XC    TCID#1,TCID#1                                                    
         XC    TCID#2,TCID#2                                                    
         XC    X,X                 CLEAR USER INDEX                             
*                                                                               
ANALOOP  XC    FUL,FUL                                                          
         GOTO1 =V(DATAMGR),DMCB,(X'08',=C'IND'),PRTQID,X,Q,BUFF                 
         CLI   DMCB+8,0                                                         
         BE    ANARCI                                                           
         ST    R1,FUL              SET EOF/ERR CONDITION                        
         TM    DMCB+8,X'40'                                                     
         BZ    *+6                                                              
         DC    H'0'                DIE IF DISK ERROR                            
         MVC   P,SPACES                                                         
         TM    DMCB+8,X'80'        END OF INDEX                                 
         BO    ANAPRTA                                                          
         TM    DMCB+8,X'10'        REC NOT FOUND                                
         BZ    *+14                                                             
         MVC   P(30),=CL30'*** INDEX REC NOT FOUND ***'                         
         B     ANAPRTA                                                          
         DC    H'0'                                                             
*                                                                               
ANARCI   ICM   R0,7,X+27           R0=..0TTTTT                                  
         SLL   R0,12                                                            
         ST    R0,DSKADR                                                        
         MVI   DSKADR+3,1          DSKADR=TTTTT001                              
         SRL   R0,12                                                            
         ST    R0,FRSTCI           FRSTCI=000TTTTT                              
         CLI   THISPQ,C'N'                                                      
         BE    ANARCI1                                                          
         MVC   DSKADR(2),X+28                                                   
         MVC   DSKADR+2(2),=X'0100'                                             
         XC    FRSTCI(2),FRSTCI    FRSTCI=0000TTTT                              
         MVC   FRSTCI+2(2),X+28                                                 
         DROP  R4                                                               
*                                                                               
ANARCI1  L     R5,=A(BUFF1)        R5=A(FIRST CI REC IN BUFF1)                  
         USING PQRECD,R5                                                        
         GOTO1 =V(DATAMGR),DMCB1,=C'DMREAD',PRTQID,DSKADR,(R5)                  
         CLI   8(R1),0                                                          
         BNE   ANABAD                                                           
*                                                                               
ANARCI2  SR    RE,RE               RE=NUM OF PART#1 CIS                         
         SR    RF,RF               RF=NUM OF PART#2 CIS                         
         TM    PQTYPE,PQTYNEW                                                   
         BO    ANARCI3                                                          
         CLI   THISPQ,C'O'         TEST REPORT TYPE WITH PQ TYPE                
         BE    ANARCI2A                                                         
         L     R0,TTMIXS           BUMP MIS MATCH TYPE COUNT                    
         AHI   R0,1                                                             
         ST    R0,TTMIXS                                                        
         MVI   P+35,C'>'           FLAG REPORT IN PRINT LINE                    
         B     ANARCI3                                                          
ANARCI2A XC    LASTCI(2),LASTCI    LAST CI=0000TTTT                             
         MVC   LASTCI+2(2),OQCIEOR                                              
         XC    LASTTK(2),LASTTK    LAST TRK=0000TTTT                            
         MVC   LASTTK+2(2),OQDAEOR                                              
         IC    RE,OQNCI                                                         
         CHI   RE,1                                                             
         BE    ANARCI4                                                          
         IC    RF,OQNCIX                                                        
         AR    RF,RE                                                            
         AHI   RF,-1                                                            
         LA    RE,1                                                             
         B     ANARCI4                                                          
*                                                                               
ANARCI3  CLI   THISPQ,C'N'         TEST REPORT TYPE WITH PQ TYPE                
         BE    ANARCI3A                                                         
         L     R0,TTMIXS           BUMP MIS MATCH TYPE COUNT                    
         AHI   R0,1                                                             
         ST    R0,TTMIXS                                                        
         MVI   P+35,C'>'           FLAG REPORT IN PRINT LINE                    
         B     ANARCI2A                                                         
ANARCI3A ICM   R0,15,PQCIEOR       NEW PQ VALUES                                
         SRL   R0,12                                                            
         ST    R0,LASTCI           LAST CI=000TTTTT                             
         ICM   R0,15,PQDAEOR                                                    
         SRL   R0,12                                                            
         ST    R0,LASTTK           LAST TRK=000TTTTT                            
         IC    RE,PQNCI                                                         
         CHI   RE,1                                                             
         BE    ANARCI4                                                          
         IC    RF,PQNCIX                                                        
         AR    RF,RE                                                            
         AHI   RF,-1                                                            
         LA    RE,1                                                             
*                                                                               
ANARCI4  STH   RF,OPART#2          SAVE NUMBER OF CIS                           
*                                                                               
ANARCI6  LTR   RF,RF               RF=NUM OF PART#2 CIS                         
         BNZ   ANARCI7                                                          
         CLC   FRSTCI,LASTCI       IF ONLY PART#1 CIS CHECK FIRST=LAST          
         BNE   ANABAD                                                           
         L     R0,LASTTK           COMPUTE NUM OF TRACKS                        
         S     R0,LASTCI                                                        
         BM    ANABAD              IGNORE FUNNIES                               
         AHI   R0,1                                                             
         STH   R0,NUMTRKS          TRACKS USED                                  
         LH    R1,OCISZ#1                                                       
         SR    R1,R0                                                            
         STH   R1,OTRKSW           TRACKS WASTED                                
         B     ANARCI8                                                          
*                                                                               
ANARCI7  LR    R0,RE               RE=NUM OF PART#1 CIS                         
         MH    R0,OCISZ#1                                                       
         ST    R0,FULL                                                          
         LR    R0,RF               RF=NUM OF PART#2 CIS                         
         AHI   R0,-1                                                            
         BM    ANABAD              IGNORE FUNNIES                               
         MH    R0,OCISZ#2                                                       
         A     R0,FULL                                                          
         ST    R0,FULL                                                          
         L     R0,LASTTK           COMPUTE NUM TRACKS IN LAST CI                
         S     R0,LASTCI                                                        
         BM    ANALOOP             IGNORE FUNNIES                               
         AHI   R0,1                                                             
         LH    R1,OCISZ#2                                                       
         SR    R1,R0                                                            
         STH   R1,OTRKSW           TRACKS WASTED                                
         A     R0,FULL                                                          
         STH   R0,NUMTRKS          TRACKS USED                                  
*                                                                               
ANARCI8  CLC   NUMTRKS,NCISZ#1     TEST IF NEEDS PART#2 CIS                     
         BH    ANARCI9                                                          
         LH    R0,NCISZ#1                                                       
         SH    R0,NUMTRKS                                                       
         STH   R0,NTRKSW                                                        
         MVC   NPART#2,=H'0'                                                    
         B     ANATOT                                                           
*                                                                               
ANARCI9  LH    R0,NUMTRKS          COMPUTE NEW PART#2 CIS                       
         SH    R0,NCISZ#1                                                       
         SRDL  R0,32                                                            
         LH    RF,NCISZ#2                                                       
         DR    R0,RF                                                            
         STH   R1,NPART#2                                                       
         MVC   NTRKSW,=H'0'                                                     
         LTR   R0,R0               TEST EXACT NUMBER OF PART#2S                 
         BZ    ANATOT                                                           
         AHI   R1,1                NEED ONE MORE PART#2                         
         STH   R1,NPART#2                                                       
         LH    R1,NCISZ#2          COMPUTE NEW TRACKS WASTED                    
         SR    R1,R0                                                            
         STH   R1,NTRKSW                                                        
         B     ANATOT                                                           
*                                                                               
ANABAD   L     R0,TTREPS           BUMP TOTAL REPORTS                           
         AHI   R0,1                                                             
         ST    R0,TTREPS                                                        
         L     R0,TTBADS           BUMP BAD REPORTS                             
         AHI   R0,1                                                             
         ST    R0,TTBADS                                                        
         B     ANALOOP                                                          
*                                                                               
ANATOT   L     R0,TTREPS           BUMP TOTAL REPORTS                           
         AHI   R0,1                                                             
         ST    R0,TTREPS                                                        
         CLC   NUMTRKS,=H'1'                                                    
         BNE   *+16                                                             
         L     R1,TTSGLS           BUMP TOTAL SINGLE TRACK REPS                 
         AHI   R1,1                                                             
         ST    R1,TTSGLS                                                        
         CLC   NUMTRKS,=H'2'                                                    
         BNE   *+16                                                             
         L     R1,TTDBLS           BUMP TOTAL DOUBLE TRACK REPS                 
         AHI   R1,1                                                             
         ST    R1,TTDBLS                                                        
         L     R0,TTTRKS           BUMP TOTAL TRACKS USED                       
         AH    R0,NUMTRKS                                                       
         ST    R0,TTTRKS                                                        
*                                                                               
ANATOT1  L     R0,TOTRKSW          BUMP TOTAL OLD TRACKS WASTED                 
         AH    R0,OTRKSW                                                        
         ST    R0,TOTRKSW                                                       
         L     R0,TNTRKSW          BUMP TOTAL NEW TRACKS WASTED                 
         AH    R0,NTRKSW                                                        
         ST    R0,TNTRKSW                                                       
         L     R0,TOPART#2         BUMP TOTAL OLD PART#2 CI COUNT               
         AH    R0,OPART#2                                                       
         ST    R0,TOPART#2                                                      
         L     R0,TNPART#2         BUMP TOTAL NEW PART#2 CI COUNT               
         AH    R0,NPART#2                                                       
         ST    R0,TNPART#2                                                      
*                                                                               
ANATOT2  TM    PQSTAT,PQSTLIVE     TEST LIVE REPORT                             
         BZ    ANATOT3                                                          
         L     R0,TCIL#1           BUMP LIVE PART#1 CI COUNT                    
         AHI   R0,1                                                             
         ST    R0,TCIL#1                                                        
         L     R0,TCIL#2           BUMP LIVE PART#2 CI COUNT                    
         AH    R0,OPART#2                                                       
         ST    R0,TCIL#2                                                        
         B     ANAPRT                                                           
ANATOT3  L     R0,TCID#1           BUMP DEAD PART#1 CI COUNT                    
         AHI   R0,1                                                             
         ST    R0,TCID#1                                                        
         L     R0,TCID#2           BUMP DEAD PART#2 CI COUNT                    
         AH    R0,OPART#2                                                       
         ST    R0,TCID#2                                                        
*                                                                               
ANAPRT   CLI   PFLAG,C'Y'          TEST IF PRINTING DETAIL                      
         BNE   ANAPRT4                                                          
         GOTO1 =V(HEXOUT),PARM,X+00,P+00,8,=C'TOG'                              
         GOTO1 =V(HEXOUT),PARM,X+27,P+17,3,=C'TOG'                              
*                                                                               
         MVC   P+24(3),PQSUBID     REPORT SUB ID                                
         MVI   P+27,C','                                                        
         SR    R0,R0                                                            
         ICM   R0,3,PQREPNO        REPORT NUMBER                                
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+28(5),DUB                                                      
         MVI   P+34,C'L'           REPORT STATUS                                
         TM    PQSTAT,PQSTLIVE                                                  
         BNZ   *+8                                                              
         MVI   P+34,C'D'                                                        
*                                  EXCEPTION FLAGS ALREADY IN P+35              
         LA    R7,P+36                                                          
         EDIT  (B2,NUMTRKS),(5,0(R7)),ZERO=NOBLANK                              
         LA    R7,6(R7)                                                         
         EDIT  (B2,OPART#2),(5,0(R7)),ZERO=NOBLANK                              
         LA    R7,6(R7)                                                         
         EDIT  (B2,OTRKSW),(5,0(R7)),ZERO=NOBLANK                               
         LA    R7,6(R7)                                                         
         EDIT  (B2,NPART#2),(5,0(R7)),ZERO=NOBLANK                              
         LA    R7,6(R7)                                                         
         EDIT  (B2,NTRKSW),(5,0(R7)),ZERO=NOBLANK                               
         LA    R7,6(R7)                                                         
*                                                                               
ANAPRT1  CLI   THISPQ,C'O'         OUTPUT REPORT PAGES/LINES/AVGCPL             
         BE    ANAPRT2                                                          
         EDIT  (B2,PQPAGES),(5,0(R7)),ZERO=NOBLANK                              
         LA    R7,6(R7)                                                         
         EDIT  (B3,PQLINES),(5,0(R7)),ZERO=NOBLANK                              
         LA    R7,6(R7)                                                         
         EDIT  (B2,PQAVCPL),(5,0(R7)),ZERO=NOBLANK                              
         LA    R7,6(R7)                                                         
         B     ANAPRT3                                                          
*                                                                               
ANAPRT2  EDIT  (B2,OQPAGES),(5,0(R7)),ZERO=NOBLANK                              
         LA    R7,6(R7)                                                         
         EDIT  (B3,OQLINES),(5,0(R7)),ZERO=NOBLANK                              
         LA    R7,6(R7)                                                         
         EDIT  (B2,OQAVCPL),(5,0(R7)),ZERO=NOBLANK                              
         LA    R7,6(R7)                                                         
         B     ANAPRT3                                                          
*                                                                               
ANAPRT3  GOTO1 =V(PRINTER)         PRINT REPORT DETAIL LINE                     
*                                                                               
ANAPRT4  OC    FUL,FUL                                                          
         BZ    ANALOOP                                                          
*                                                                               
ANAPRTA  MVC   MID1(84),ANAEND1    PRINT TOTAL LINE                             
         MVC   MID2(84),ANAEND2                                                 
         ZAP   LINE,=P'99'                                                      
*                                                                               
         EDIT  (B4,TTREPS),(5,P+00),ZERO=NOBLANK                                
         EDIT  (B4,TTSGLS),(5,P+06),ZERO=NOBLANK                                
         EDIT  (B4,TTDBLS),(5,P+12),ZERO=NOBLANK                                
         EDIT  (B4,TTBADS),(5,P+18),ZERO=NOBLANK                                
         EDIT  (B4,TTMIXS),(5,P+24),ZERO=NOBLANK                                
*                                                                               
         LA    R7,P+36                                                          
         EDIT  (B4,TTTRKS),(5,0(R7)),ZERO=NOBLANK                               
         LA    R7,6(R7)                                                         
         EDIT  (B4,TOPART#2),(5,0(R7)),ZERO=NOBLANK                             
         LA    R7,6(R7)                                                         
         EDIT  (B4,TOTRKSW),(5,0(R7)),ZERO=NOBLANK                              
         LA    R7,6(R7)                                                         
         EDIT  (B4,TNPART#2),(5,0(R7)),ZERO=NOBLANK                             
         LA    R7,6(R7)                                                         
         EDIT  (B4,TNTRKSW),(5,0(R7)),ZERO=NOBLANK                              
         GOTO1 =V(PRINTER)                                                      
         MVC   P(85),ANAEND3                                                    
         GOTO1 =V(PRINTER)                                                      
ANAPRTX  OC    FUL,FUL                                                          
         BZ    ANALOOP                                                          
*                                                                               
ANAT     GOTO1 =V(PRINTER)                                                      
         MVC   P(35),=CL35'PRTQX CI TOTALS OLD=NN/NN NEW=NN/NN'                 
         MVC   P(5),PRTQID                                                      
         LH    R0,OCISZ#1                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+20(2),DUB                                                      
         LH    R0,OCISZ#2                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+23(2),DUB                                                      
         LH    R0,NCISZ#1                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+30(2),DUB                                                      
         LH    R0,NCISZ#2                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+33(2),DUB                                                      
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
         LA    R7,P                                                             
*                                                                               
         MVC   0(7,R7),=C'TOTL#1='                                              
         SR    R0,R0                                                            
         ICM   R0,3,CICITOT                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(5,R7),DUB                                                      
         LA    R7,13(R7)                                                        
*                                                                               
         MVC   0(7,R7),=C'LIVE#1='                                              
         L     R0,TCIL#1                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(5,R7),DUB                                                      
         LA    R7,13(R7)                                                        
*                                                                               
         MVC   0(7,R7),=C'DEAD#1='                                              
         L     R0,TCID#1                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(5,R7),DUB                                                      
         LA    R7,13(R7)                                                        
*                                                                               
         MVC   0(7,R7),=C'TOTL#2='                                              
         SR    R0,R0                                                            
         ICM   R0,3,CJCITOT                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(5,R7),DUB                                                      
         LA    R7,13(R7)                                                        
*                                                                               
         MVC   0(7,R7),=C'LIVE#2='                                              
         L     R0,TCIL#2                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(5,R7),DUB                                                      
         LA    R7,13(R7)                                                        
*                                                                               
         MVC   0(7,R7),=C'DEAD#2='                                              
         L     R0,TCID#2                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(5,R7),DUB                                                      
         LA    R7,13(R7)                                                        
*                                                                               
ANATX    GOTO1 =V(PRINTER)                                                      
         B     ANAX                                                             
*                                                                               
ANAERR   MVC   P(24),=CL24'INVALID ANALYZE REQUEST'                             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
ANAX     MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   TITLE+26(5),SPACES                                               
         XIT1                                                                   
         DROP  R5                                                               
         LTORG                                                                  
ANAMID1  DC    CL36'REPORT KEY       0TTTTT REPORT ID S '                       
         DC    CL48'#TRKS   OLD   OLD   NEW   NEW PAGES LINES AVCPL '           
ANAMID2  DC    CL36'---------------- ------ --------- - '                       
         DC    CL48'----- #2CIS WASTE #2CIS WASTE ----- ----- ----- '           
ANAEND1  DC    CL36'TOTAL 1-TRK 2-TRK  BAD   MIX        '                       
         DC    CL48'#TRKS   OLD   OLD   NEW   NEW       '                       
ANAEND2  DC    CL36'----- ----- ----- ----- ----- ----- '                       
         DC    CL48'----- #2CIS WASTE #2CIS WASTE'                              
ANAEND3  DC    CL36'----- ----- ----- ----- ----- ----- '                       
         DC    CL48'----- ----- ----- ----- -----'                              
                                                                                
         DS    0D                                                               
         DC    C'*CIATAB*'                                                      
CIATAB   DS    64000F                                                           
CIATABX  DC    8X'FF'                                                           
                                                                                
         DS    0D                                                               
         DC    C'*PURTAB*'                                                      
PURTAB   DS    1000XL12'00'                                                     
PURTABX  DC    8X'FF'                                                           
                                                                                
         DC    C'*WRKWRK*'                                                      
PQWORK   DS    20000D                                                           
                                                                                
         DS    0D                                                               
         DC    CL8'SSB*SSB*'                                                    
SSB      DC    X'0000FF',X'40',4X'00',CL8' ',32X'00',A(MASTC),204X'00'          
UTL      DC    F'0',X'01',XL3'00',XL248'00'                                     
MASTC    DC    1932X'00'                                                        
MCARC    DC    CL72' '                                                          
         DC    44X'00'                                                          
         EJECT                                                                  
*DNPRTQD                                                                        
       ++INCLUDE DNPRTQD                                                        
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
         EJECT                                                                  
SSOD     DSECT                                                                  
*FASSBOFF                                                                       
       ++INCLUDE FASSBOFF                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009DMPRTQT   12/15/16'                                      
         END                                                                    
