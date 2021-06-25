*          DATA SET ACREPXV05  AT LEVEL 158 AS OF 06/15/99                      
*PHASE ACXV02C                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE DICTATE                                                                
*INCLUDE SCANNER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'FIX GENDIR/GENFILE RFP ELEMENTS'                                
ACXV02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACXV**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACX4D,RC                                                         
         EJECT                                                                  
         CLI   MODE,REQFRST                                                     
         BNE   XIT                                                              
         ZAP   TOTRECS,=P'0'                                                    
         ZAP   TOTCHGS,=P'0'                                                    
         ZAP   CMPRECS,=P'0'                                                    
         ZAP   CMPCHGS,=P'0'                                                    
         MVI   FORCEHED,YES                                                     
         MVI   FCRESET,YES                                                      
         MVI   FILTSEL,0                                                        
         GOTO1 HEXIN,DMCB,QSELECT,FILTSEL,2                                     
         OC    DMCB+12(4),DMCB+12                                               
         BNZ   *+8                                                              
         MVI   FILTSEL,0                                                        
         XC    XKEY,XKEY                                                        
         XC    RQSTART,RQSTART                                                  
         MVC   RQEND,=X'FFFFFF'                                                 
         CLC   QSTART,SPACES                                                    
         BNH   DT10                                                             
         GOTO1 DATCON,DMCB,(0,QSTART),(1,RQSTART)                               
*                                                                               
DT10     CLC   QEND,SPACES                                                      
         BNH   DT20                                                             
         GOTO1 DATCON,DMCB,(0,QEND),(1,RQEND)                                   
*                                                                               
         USING GRPKEYD,R3                                                       
DT20     LA    R3,XKEY                                                          
         XC    XKEY,XKEY                                                        
         XC    XDIR,XDIR                                                        
         MVI   GRPKSYS,GRPKSYSQ                                                 
         MVI   GRPKSTYP,GRPKSTYQ                                                
*                                                                               
         USING MASTD,R5                                                         
         L     R5,ADMASTC                                                       
         L     R2,MCUTL                                                         
         MVC   SAVSE,4(R2)                                                      
         MVI   4(R2),X'0A'                                                      
         GOTO1 DATAMGR,DMCB,(X'80',DMRDHI),=CL8'GENDIR',XKEY,XDIR               
         TM    DMCB+8,0                                                         
         BZ    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         LA    R3,XDIR                                                          
RS03     CLC   =X'002F',XDIR       CHECK RECORD TYPE                            
         BNE   RS90                FINISHED                                     
         CLI   GRPKSYST,C'A'       ACCOUNTING                                   
         BNE   RS80                                                             
         CLC   GRPKAGY,=C'YN'                                                   
         BNE   RS80                                                             
*        CLC   GRPKUSER,=AL2(4766)                                              
*        BNE   RS80                                                             
         MVC   DA,XDIR+36                                                       
         MVC   SVDA,DA                                                          
         LA    R3,IO                                                            
         GOTO1 DATAMGR,DMCB,(X'80',DMGET),=CL8'GENFILE',DA,(R3),XWRK            
         MVI   WRTSW,NO            SET WRITE SWITCH                             
         MVI   DMPSW,NO                                                         
*                                                                               
RS24     LA    R4,GRPFSTEL                                                      
         MVC   GROUPNME,GRPKGRP                                                 
RS30     CLI   0(R4),0                                                          
         BE    RS70                                                             
         CLI   0(R4),GRPHCDQ       X'01' HEADER ELEMENT                         
         BE    ELEM01                                                           
         CLI   0(R4),GRPRCDQ       X'30' REQ CARD ELEMENT                       
         BE    ELEM30                                                           
*                                                                               
RS35     SR    R1,R1                                                            
         IC    R1,1(,R4)                                                        
         AR    R4,R1                                                            
         B     RS30                                                             
         EJECT                                                                  
         USING GRPHD,R4                                                         
*LEM01   CLC   GRPHDEST,=AL2(4766)                                              
*        BNE   RS35                                                             
*LEM01   SR    R8,R8                                                            
*        IC    R8,1(R4)                                                         
*        GOTO1 PRNTBL,DMCB,=C'HEADER',(R4),C'DUMP',(R8),=C'2D'                  
ELEM01   B     RS35                                                             
         DROP  R4                                                               
         EJECT                                                                  
         USING GRPRD,R4                                                         
ELEM30   DS    0H                                                               
*&&DO                                                                           
         CLI   GRPRRNUM,C'R'                                                    
         BE    ELEM31                                                           
         CLI   GRPRRNUM,C'I'                                                    
         BE    ELEM31                                                           
         CLI   GRPRRNUM,C'X'                                                    
         BE    ELEM31                                                           
         CLI   GRPRRNUM,C'P'                                                    
         BE    ELEM31                                                           
         CLI   GRPRRNUM,C'1'                                                    
         BE    ELEM31                                                           
         CLI   GRPRRNUM,C'2'                                                    
         BE    ELEM31                                                           
         CLI   GRPRRNUM,C'G'                                                    
         BE    ELEM31                                                           
         CLI   GRPRRNUM,C'B'                                                    
         BE    ELEM31                                                           
         CLI   GRPRRNUM,C'V'                                                    
         BNE   RS35                                                             
*                                                                               
ELEM31   CLI   GRPRRNUM+1,C'L'                                                  
         BE    *+8                                                              
         CLI   GRPRRNUM+1,C'P'                                                  
         BNE   RS35                                                             
*&&                                                                             
         USING RQHDR,R6                                                         
         LA    R6,GRPCARD          POINT AT CARDS 0 - 2                         
         CLI   GRPRCRDN,0                                                       
         BNE   ELEM32                                                           
         MVC   CARD0,GRPCARD                                                    
         B     RS35                                                             
*                                                                               
         USING ACQCARD1,R6                                                      
ELEM32   LA    R6,GRPCARD          POINT AT CARD 1                              
         CLI   GRPRCRDN,1                                                       
         BNE   ELEM34                                                           
         MVC   CARD1,GRPCARD                                                    
         CLC   GRPRRNUM(2),=C'RW'                                               
         BE    ELEM37                                                           
         CLC   ACQUNT(2),=C'SR'    Account rcv                                  
         BE    ELEM37                                                           
         B     RS35                                                             
*                                                                               
         USING ACQCARD2,R6                                                      
ELEM34   LA    R6,GRPCARD          POINT AT CARD 2                              
         CLI   GRPRCRDN,2                                                       
         BNE   RS35                NEXT ELEMENT                                 
         MVC   CARD2,GRPCARD                                                    
         CLC   GRPRRNUM(2),=C'RW'  ACC WRITER ?                                 
         BE    ELEM37                                                           
         L     R8,=A(OFFTAB)                                                    
         CLI   ACQOFFFL,X'3B'                                                   
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   ACQOFFFL,X'2E'                                                   
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   ACQOFFFL,X'0C'                                                   
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   ACQOFFFL,SPACES                                                  
         BNH   RS35                                                             
*                                                                               
ELEM35   CLI   0(R8),X'FF'         END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   ACQOFFFL+1,C' '                                                  
         BH    RS35                                                             
         CLC   ACQOFFFL(1),0(R8)                                                
         BE    ELEM36                                                           
         LA    R8,3(,R8)                                                        
         B     ELEM35                                                           
*                                                                               
         USING ACQCARD2,R6                                                      
ELEM36   MVC   ACQOFFFL,1(R8)                                                   
         B     ELEM36W                                                          
*                                                                               
         USING ACQCARD1,R6                                                      
ELEM36B  MVC   ACQACT+1(2),1(R8)                                                
*                                                                               
ELEM36W  MVI   WRTSW,YES           SET WRITE SWITCH                             
*                                                                               
         USING RQHDR,R8                                                         
ELEM37   LA    R8,CARD0                                                         
         MVC   P,SPACES                                                         
         CLC   GRPRRNUM(2),=C'RW'  ACC WRITER ?                                 
         BE    ELEM37A                                                          
         MVC   P+1(4),=C'NEW='                                                  
         MVC   P+5(2),ACQOFFFL                                                  
*                                                                               
         USING ACQCARD2,R6                                                      
         LA    R6,CARD2                                                         
         MVC   P+8(4),=C'OLD='                                                  
         MVC   P+12(1),ACQOFFFL                                                 
*                                                                               
ELEM37A  MVC   P+15(2),GRPRRNUM                                                 
*        MVC   P+1(7),=C'FORMAT='                                               
*        MVC   P+8(8),ACQAPPL                                                   
         MVC   P+20(7),=C'OUTPUT='                                              
         MVC   P+27(6),RQHOUT                                                   
*        CLC   RQHOUT,SPACES                                                    
*        BNH   RS35                                                             
*        CLC   RQHOUT,=CL6'DOWN'                                                
*        BE    RS35                                                             
*        CLC   RQHOUT,=CL6'DIRECT'                                              
*        BE    RS35                                                             
*        CLC   =C'A/R2B',ACQAPPL                                                
*        BNE   RS35                                                             
         MVC   P+38(6),=C'GROUP='                                               
         MVC   P+44(8),GRPKGRP                                                  
         MVC   P+55(4),=C'ID#='                                                 
         SR    R1,R1                                                            
         ICM   R1,3,GRPKUSER                                                    
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+59(4),DUB                                                      
         MVC   P+70(4),=C'ADR='                                                 
         GOTO1 HEXOUT,DMCB,SVDA,P+74,4                                          
*                                                                               
         USING ACQCARD1,R6                                                      
         LA    R6,CARD1                                                         
         MVC   P+84(4),=C'ACT='                                                 
         MVC   P+88(14),ACQUNT                                                  
         GOTO1 ACREPORT                                                         
         MVC   P,SPACES                                                         
*        LA    R8,80               ELEMENT LENGTH                               
*        GOTO1 PRNTBL,DMCB,=C'CARD1',CARD1,C'DUMP',(R8),=C'2D'                  
         B     RS35                                                             
*&&DO                                                                           
ELEM33   CLC   ACQSEL+4(2),=C'M '                                               
         BNE   ELEM34                                                           
         MVC   ACQTTYPE(2),ACQSEL+4                                             
         B     ELEM50                                                           
*                                                                               
ELEM34   MVI   ACQTTYPE,C'0'                                                    
         MVC   ACQTTYPE+1(2),ACQSEL+4                                           
         B     ELEM50                                                           
                                                                                
ELEM40   CLC   ACQSEL(3),=C'TYP'                                                
         BNE   RS35                MUST BE OK                                   
         CLI   QOPT2,C'Y'                                                       
         BNE   ELEM43                                                           
         LA    R8,80               ELEMENT LENGTH                               
         GOTO1 PRNTBL,DMCB,=C'CARD1',CARD1,C'DUMP',(R8),=C'2D'                  
*                                                                               
ELEM43   MVC   ACQTTYPE,ACQSEL                                                  
*                                                                               
ELEM50   MVC   ACQSEL,SPACES                                                    
         TM    ACQTTYPE+1,X'40'    EXCLUDE FACTOR ?                             
         BO    ELEM52                                                           
         OI    ACQTTYPE+1,X'40'    MOVE THE INDICATION                          
         NI    ACQTTYPE,X'FF'-X'40'                                             
*                                                                               
ELEM52   CLI   QOPT2,C'Y'                                                       
         BNE   RS35                                                             
         CLI   GRPRCRDN,1                                                       
         BNE   *+10                                                             
         MVC   GRPCARD(L'CARD1),CARD1                                           
*        CLI   GRPRCRDN,2                                                       
*        BNE   *+10                                                             
*        MVC   GRPCARD(L'CARD2),CARD2                                           
         LA    R8,80                                                            
         GOTO1 PRNTBL,DMCB,=C'NEW',CARD1,C'DUMP',(R8),=C'2D'                    
         MVI   WRTSW,YES           SET WRITE SWITCH                             
         B     RS35                                                             
*                                                                               
RS52     SR    R8,R8                                                            
         IC    R8,1(R4)            ELEMENT LENGTH                               
         GOTO1 PRNTBL,DMCB,=C'HEADER',GRPKAGY,C'DUMP',25,=C'2D'                 
         GOTO1 PRNTBL,DMCB,=C'REQUEST',(R4),C'DUMP',(R8),=C'2D'                 
         GOTO1 PRNTBL,DMCB,=C'BEFORE',ACQAPPL,C'DUMP',12,=C'2D'                 
         LA    R1,L'ACQAPPL                                                     
         LA    RF,ACQAPPL                                                       
RS55     SR    RE,RE                                                            
         ICM   RE,1,0(RF)                                                       
         BZ    RS56                                                             
         SH    RE,=H'77'                                                        
         BP    RS55A                                                            
         SR    RE,RE                                                            
         ICM   RE,1,0(RF)                                                       
         SH    RE,=H'66'                                                        
         BP    RS55A                                                            
         DC    H'00'                                                            
*                                                                               
RS55A    AH    RE,=Y(ESCHIGHQ)                                                  
         STC   RE,0(RF)                                                         
RS56     LA    RF,1(,RF)                                                        
         BCT   R1,RS55                                                          
         MVC   GRPCARD,CARD2                                                    
*                                                                               
RS60     SR    R8,R8                                                            
         IC    R8,1(R4)            ELEMENT LENGTH                               
         GOTO1 PRNTBL,DMCB,=C'AFTER',ACQAPPL,C'DUMP',12,=C'2D'                  
         GOTO1 PRNTBL,DMCB,=C'CHANGED',(R4),C'DUMP',(R8),=C'2D'                 
         AP    TOTCHGS,=P'1'                                                    
         BAS   RE,DUMP                                                          
         MVI   WRTSW,YES           SET WRITE SWITCH                             
         MVI   DMPSW,YES                                                        
         B     RS35                NEXT ELEMENT                                 
         DROP  R4,R6                                                            
*&&                                                                             
         EJECT                                                                  
RS70     CLI   WRTSW,YES                                                        
         BNE   RS80                                                             
         AP    CMPCHGS,=P'1'                                                    
         CLI   RCWRITE,YES                                                      
         BNE   RS80                                                             
         AP    TOTCHGS,=P'1'                                                    
         LA    R3,IO                                                            
         GOTO1 DATAMGR,DMCB,DMPUT,=CL8'GENFILE',DA,(R3),XWRK                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
RS80     GOTO1 DATAMGR,DMCB,(X'80',DMRSEQ),=CL8'GENDIR',XKEY,XDIR               
         AP    CMPRECS,=P'1'                                                    
         AP    TOTRECS,=P'1'                                                    
         B     RS03                                                             
         DROP  R3                                                               
         EJECT                                                                  
*              REST MONACC KEY                                                  
*                                                                               
RS90     EQU   *                                                                
         L     R5,ADMASTC                                                       
         USING MASTD,R5                                                         
         L     R2,MCUTL                                                         
         MVC   4(1,R2),SAVSE       RESTORE SE                                   
*        MVI   4(R2),X'B6'         SE # FOR ACCB                                
*        USING ACMD,R5                                                          
*        L     R5,AMONACC                                                       
*        MVC   ACKEY,SPACES                                                     
*        MVC   ACKEY,ACMLKEY                                                    
*        GOTO1 DATAMGR,DMCB,DMREAD,=CL8'ACCOUNT',ACKEY,IO,0                     
*        GOTO1 DATAMGR,DMCB,DMREAD,=CL8'ACCDIR',ACKEY,ACKEY                     
*        CLI   DMCB+8,0                                                         
*        BE    *+6                                                              
*        DC    H'00'                                                            
*                                                                               
         MVC   P(5),=C'COMP='                                                   
         MVC   CMPCDE,PRVKEY+2                                                  
         GOTO1 HEXOUT,DMCB,CMPCDE,P+5,1                                         
         OI    CMPCHGS+7,X'0F'                                                  
         MVC   P+10(16),=C'RECORDS CHANGED='                                    
         UNPK  P+26(8),CMPCHGS                                                  
         OI    CMPRECS+7,X'0F'                                                  
         MVC   P+37(14),=C'TOTAL RECORDS='                                      
         UNPK  P+51(8),CMPRECS                                                  
         GOTO1 ACREPORT                                                         
RS95     MVC   P(6),=C'TOTALS'                                                  
         OI    TOTCHGS+7,X'0F'                                                  
         MVC   P+10(16),=C'RECORDS CHANGED='                                    
         UNPK  P+26(8),TOTCHGS                                                  
         OI    TOTRECS+7,X'0F'                                                  
         MVC   P+37(14),=C'TOTAL RECORDS='                                      
         UNPK  P+51(8),TOTRECS                                                  
         GOTO1 ACREPORT                                                         
*        GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',MNFLKEY,IO                       
*        CLI   DMCB+8,0                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
*&&DO                                                                           
         USING ACKEYD,R3                                                        
*              ROUTINE TO GET AN ELEMENT                                        
         SPACE 1                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
DELEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'D',=C'ACCBIG '),((R4),(R2)),((R5),(R3))            
         B     XIT                                                              
         SPACE 1                                                                
*              ROUTINE TO ADD AN ELEMENT                                        
         SPACE 1                                                                
*              P1   A(RECORD)                                                   
*              P2   A(ELEMENT)                                                  
         SPACE 1                                                                
ADDEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         GOTO1 HELLO,DMCB,(C'P',=C'ACCBIG '),(R2),(R3)                          
         CLI   DMCB+12,0                                                        
         BE    XIT                                                              
         TM    DMCB+12,X'05'                                                    
         BNZ   *+6                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         MVC   P(30),=CL30'RECORD TOO BIG TO ADD TO'                            
         GOTO1 ACREPORT                                                         
         MVI   WRTSW,NO                                                         
         B     XIT                                                              
         EJECT                                                                  
*&&                                                                             
DUMP     NTR1                                                                   
         CLI   DMPSW,YES                                                        
         BE    XIT                                                              
         CLI   QOPT1,YES           MUST REQUEST DUMP                            
         BNE   XIT                                                              
         SR    R8,R8                                                            
         ICM   R8,3,32(R3)                                                      
         GOTO1 PRNTBL,DMCB,=C'RFP',(R3),C'DUMP',(R8),=C'2D'                     
         B     XIT                                                              
DMPUT    DC    CL8'PUTREC'                                                      
         EJECT                                                                  
TURNOFF  EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
EOT      EQU   0                   END OF TABLE                                 
*                                                                               
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'500'                                                         
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
VDICTAT  DC    V(DICTATE)                                                       
WRTSW    DS    CL1                                                              
DMPSW    DS    CL1                                                              
         DC    C'**XKEY**'                                                      
XKEY     DS    CL32                                                             
         DC    C'**XDIR**'                                                      
XDIR     DS    CL60                                                             
         EJECT                                                                  
HEXIN    DC    V(HEXIN)                                                         
         LTORG                                                                  
         DS    0D                                                               
         DC    C'***IO***'                                                      
IO       DS    CL2100                                                           
*                                                                               
OFFTAB   DC    C'AAA'                                                           
         DC    C'BD3'                                                           
         DC    C'CCC'                                                           
         DC    C'DBD'                                                           
         DC    C'ECE'                                                           
         DC    C'FCF'                                                           
         DC    C'GCG'                                                           
         DC    C'HCH'                                                           
         DC    C'ICI'                                                           
         DC    C'JCJ'                                                           
         DC    C'KCK'                                                           
         DC    C'LCL'                                                           
         DC    C'MBM'                                                           
         DC    C'NCN'                                                           
         DC    C'OCO'                                                           
         DC    C'PCP'                                                           
         DC    C'QCQ'                                                           
         DC    C'RCR'                                                           
         DC    C'SCS'                                                           
         DC    C'TCT'                                                           
         DC    C'UCU'                                                           
         DC    C'VCV'                                                           
         DC    C'WBW'                                                           
         DC    C'XBX'                                                           
         DC    C'YCY'                                                           
         DC    C'ZCZ'                                                           
         DC    C'0C0'                                                           
         DC    C'1C1'                                                           
         DC    C'2C2'                                                           
         DC    C'3C3'                                                           
         DC    C'4C4'                                                           
         DC    C'5C5'                                                           
         DC    C'6C6'                                                           
         DC    C'7C7'                                                           
         DC    C'8C8'                                                           
         DC    C'9C9'                                                           
         DC    C'#BA'                                                           
         DC    C'>BB'                                                           
         DC    C'<D1'                                                           
         DC    C'aaA'                                                           
         DC    C'bd3'                                                           
         DC    C'ccC'                                                           
         DC    C'dbD'                                                           
         DC    C'ecE'                                                           
         DC    C'fcF'                                                           
         DC    C'gcG'                                                           
         DC    C'hcH'                                                           
         DC    C'icI'                                                           
         DC    C'jcJ'                                                           
         DC    C'kcK'                                                           
         DC    C'lcL'                                                           
         DC    C'mbM'                                                           
         DC    C'ncN'                                                           
         DC    C'ocO'                                                           
         DC    C'pcP'                                                           
         DC    C'qcQ'                                                           
         DC    C'rcR'                                                           
         DC    C'scS'                                                           
         DC    C'tcT'                                                           
         DC    C'ucU'                                                           
         DC    C'vcV'                                                           
         DC    C'wbW'                                                           
         DC    C'xbX'                                                           
         DC    C'ycY'                                                           
         DC    C'zcZ'                                                           
         DC    X'B0',C'c0'                                                      
         DC    X'B1',C'c1'                                                      
         DC    X'B2',C'c2'                                                      
         DC    X'B3',C'c3'                                                      
         DC    X'B4',C'c4'                                                      
         DC    X'B5',C'c5'                                                      
         DC    X'B6',C'c6'                                                      
         DC    X'B7',C'c7'                                                      
         DC    X'B8',C'c8'                                                      
         DC    X'B9',C'c9'                                                      
         DC    X'3B',C'bA'                                                      
         DC    X'2E',C'bB'                                                      
         DC    X'0C',C'd1'                                                      
         DC    X'FF'                                                            
         EJECT                                                                  
ACX4D    DSECT                                                                  
RQSTART  DS    PL3                                                              
RQEND    DS    PL3                                                              
FILTSEL  DS    XL1                                                              
NEWTYPE  DS    CL1                                                              
NEWCODE  DS    CL6                                                              
SAVELDG  DS    CL2                                                              
CMPCDE   DS    XL1                                                              
CMPRECS  DS    PL8                                                              
CMPCHGS  DS    PL8                                                              
TOTRECS  DS    PL8                                                              
TOTCHGS  DS    PL8                                                              
ELM      DS    CL255                                                            
MNFLKEY  DS    CL42                                                             
ACKEY    DS    CL64                                                             
DA       DS    XL4                                                              
SVDA     DS    XL4                                                              
XWRK     DS    12D                                                              
PRVKEY   DS    CL42                                                             
SAVSE    DS    XL1                                                              
NPARMS   DS    XL1                 # OF PARAMETERS                              
BLOCK    DS    6CL32               DATA BLOCK FOR SCANNER                       
DUMFLDH  DS    CL8                 DUMMY FIELD HEADER                           
DUMFLD   DS    CL64                                                             
REPTYPE  DS    CL4                                                              
FOUND    DS    CL1                                                              
COST     DS    CL1                                                              
BNR      DS    CL1                                                              
GROUPNME DS    CL8                                                              
CARD0    DS    CL80                                                             
CARD1    DS    CL80                                                             
CARD2    DS    CL80                                                             
ESCHIGHQ EQU   48                                                               
         EJECT                                                                  
*  ACREPWORKD                                                                   
*  ACGENFILE                                                                    
*  ACGENMODES                                                                   
*  DMDTFIS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE DMDTFIS                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE ACDDEQUS                                                       
       ++INCLUDE ACQD                                                           
       ++INCLUDE CTGENRFP                                                       
RQHDR  DSECT                                                                    
       ++INCLUDE DMREQHDRA                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'158ACREPXV05 06/15/99'                                      
         END                                                                    
