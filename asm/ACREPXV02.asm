*          DATA SET ACREPXV02  AT LEVEL 070 AS OF 05/01/02                      
*PHASE ACXV02A                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'SCRIBE FORMAT COUNT BY TYPE'                                    
ACXV02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACXV**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACXVD,RC                                                         
         EJECT                                                                  
         CLI   MODE,REQFRST                                                     
         BNE   XIT                                                              
         ZAP   TOTRECS,=P'0'                                                    
         ZAP   TOTCHGS,=P'0'                                                    
         ZAP   CMPRECS,=P'0'                                                    
         ZAP   CMPRECS,=P'0'                                                    
         ZAP   RCVFMTS,=P'0'                                                    
         ZAP   INCFMTS,=P'0'                                                    
         ZAP   PAYFMTS,=P'0'                                                    
         ZAP   EXPFMTS,=P'0'                                                    
         ZAP   PRDFMTS,=P'0'                                                    
         ZAP   CSTFMTS,=P'0'                                                    
         ZAP   CSHFMTS,=P'0'                                                    
         ZAP   GLDFMTS,=P'0'                                                    
         ZAP   UNKFMTS,=P'0'                                                    
         ZAP   TXTRECS,=P'0'                                                    
         ZAP   NOTYTOT,=P'0'                                                    
         ZAP   NOTYCPY,=P'0'                                                    
         ZAP   FMTDELT,=P'0'                                                    
         MVI   FORCEHED,YES                                                     
         MVI   FCRESET,YES                                                      
         MVI   FILTSEL,0                                                        
         GOTO1 HEXIN,DMCB,QSELECT,FILTSEL,2                                     
         OC    DMCB+12(4),DMCB+12                                               
         BNZ   *+8                                                              
         MVI   FILTSEL,0                                                        
         XC    TEXTMSG,TEXTMSG                                                  
         XC    XKEY,XKEY                                                        
*                                                                               
         USING RESRECD,R3                                                       
         LA    R3,XKEY                                                          
         MVI   RESKTYP,RESKTYPQ    SET KEY FOR RS RECORDS                       
         MVI   RESKSUB,RESKSUBQ                                                 
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',XKEY,IO                          
         LA    R3,IO                                                            
*                                                                               
RS03     CLC   IO(2),XKEY          CHECK RECORD TYPE                            
         BNE   RS90                                                             
         CLC   IO(3),PRVKEY        SAME COMP CODE?                              
         BE    RS20                                                             
         BAS   RE,PRNTCPY                                                       
*                                                                               
RS20     MVC   PRVKEY,IO                                                        
         MVI   WRTSW,NO            SET WRITE SWITCH                             
         MVI   DMPSW,YES           SET DUMP SWITCH                              
*                                                                               
RS24     LA    R3,IO                                                            
         LA    R4,ACCORFST(,R3)                                                 
         MVI   TYPEOK,C'N'                                                      
         MVC   REQDATE,SPACES                                                   
         MVC   CHGDATE,SPACES                                                   
         CLC   RESRLEN,=H'1400'                                                 
         BL    RS28                                                             
         MVC   P(8),RESKFORM                                                    
         LH    RF,RESRLEN                                                       
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   P+22(4),=C'LEN='                                                 
         UNPK  P+26(8),DUB                                                      
         GOTO1 ACREPORT                                                         
*                                                                               
RS28     CLI   FILTSEL,0                                                        
         BE    RS30                                                             
         CLC   FILTSEL,2(R3)       MATCH ON COMPANY                             
         BNE   RS70                                                             
*                                                                               
RS30     CLI   0(R4),0                                                          
         BE    RS70                                                             
         CLI   0(R4),X'25'                                                      
         BE    FF10                                                             
         CLI   0(R4),X'C1'                                                      
         BE    HD10                                                             
         CLI   0(R4),X'C2'                                                      
         BE    RW10                                                             
         CLI   0(R4),X'C3'                                                      
         BE    CL10                                                             
         CLI   0(R4),X'C4'                                                      
         BE    PF10                                                             
         CLI   0(R4),X'C5'                                                      
         BE    FL10                                                             
         CLI   0(R4),X'FB'         REQUESTED DATES                              
         BE    RQ10                                                             
         CLI   0(R4),X'A1'         LAST CHANGED DATE                            
         BE    AT10                                                             
*                                                                               
RS35     SR    R1,R1                                                            
         IC    R1,1(,R4)                                                        
         AR    R4,R1                                                            
         B     RS30                                                             
         EJECT                                                                  
         USING FFNELD,R5                                                        
FF10     SR    RF,RF                                                            
         LR    R5,R4                                                            
         LA    R2,RCVFMTS                                                       
         MVI   TYPEOK,C'Y'                                                      
         CLC   FFNUMBER+2(2),=AL2(AC#RSRCV)                                     
         BE    FF20                                                             
         LA    R2,INCFMTS                                                       
         CLC   FFNUMBER+2(2),=AL2(AC#RSINC)                                     
         BE    FF20                                                             
         LA    R2,PAYFMTS                                                       
         CLC   FFNUMBER+2(2),=AL2(AC#RSPAY)                                     
         BE    FF20                                                             
         LA    R2,EXPFMTS                                                       
         CLC   FFNUMBER+2(2),=AL2(AC#RSEXP)                                     
         BE    FF20                                                             
         LA    R2,PRDFMTS                                                       
         CLC   FFNUMBER+2(2),=AL2(AC#RS497)                                     
         BE    FF20                                                             
         LA    R2,CSTFMTS                                                       
         CLC   FFNUMBER+2(2),=AL2(AC#RS498)                                     
         BE    FF20                                                             
         LA    R2,CSHFMTS                                                       
         CLC   FFNUMBER+2(2),=AL2(AC#RS540)                                     
         BE    FF20                                                             
         LA    R2,GLDFMTS                                                       
         CLC   FFNUMBER+2(2),=AL2(AC#GLG)                                       
         BE    FF20                                                             
         LA    R2,UNKFMTS                                                       
         MVC   P(8),RESKFORM                                                    
         MVC   P+10(7),=C'=UNKNOWN'                                             
         GOTO1 ACREPORT                                                         
FF20     AP    0(8,R2),=P'1'                                                    
         B     RS35                                                             
         EJECT                                                                  
         USING PACELD,R4                                                        
AT10     GOTO1 DATCON,DMCB,(1,PACDATE),(17,CHGDATE)                             
         B     RS35                                                             
         SPACE 2                                                                
         USING DTSELD,R4                                                        
RQ10     GOTO1 DATCON,DMCB,(2,DTSDATE),(17,REQDATE)                             
         B     RS35                                                             
         EJECT                                                                  
         USING RHDELD,R4                                                        
HD10     DS    0H                  HEADLINE ELEMENT                             
         B     RS35                START AGAIN                                  
         DROP  R5                                                               
         EJECT                                                                  
         USING RRWELD,R4                                                        
RW10     DS    0H                     ROW ELEMENT                               
*        CLC   =XL2'05BC',RRWDATA                                               
*        BE    *+10                                                             
*        CLC   =XL2'0B74',RRWDATA                                               
*        BNE   RS35                                                             
*        GOTO1 HEXOUT,DMCB,RRWDATA,P+2,12                                       
*        GOTO1 ACREPORT                                                         
         B     RS35                                                             
         EJECT                                                                  
         USING RCLELD,R4                                                        
CL10     DS    0H                     COLUMN ELEMENT                            
*        CLC   =XL2'05BC',RCLDATA                                               
*        BE    *+10                                                             
*        CLC   =XL2'0B74',RCLDATA                                               
*        BNE   RS35                                                             
*        GOTO1 HEXOUT,DMCB,RCLDATA,P+2,12                                       
*        GOTO1 ACREPORT                                                         
         B     RS35                                                             
         EJECT                                                                  
         USING RPFELD,R4                                                        
PF10     DS    0H                     PROFILE ELEMENT                           
         B     RS35                                                             
         EJECT                                                                  
         USING RFLELD,R4                                                        
FL10     DS    0H                     FILTER ELEMENT                            
         B     RS35                                                             
*&&DO                                                                           
         CLI   RFLTYPE,RFLLDG                                                   
         BNE   RS35                                                             
         CLI   RFLSEQ,0                                                         
         BNE   RS35                                                             
         SR    R1,R1                                                            
         IC    R1,RFLLN                                                         
         SH    R1,=Y(RFLDATA-RFLELD)                                            
         SRL   R1,1                NUMBER OF LEDGERS                            
         XC    ELM25,ELM25                                                      
         STC   R1,BYTE                                                          
         LA    R6,LDGTAB                                                        
FL12     CLI   0(R6),0                                                          
         BE    RS35                                                             
         SR    R1,R1                                                            
         IC    R1,BYTE                                                          
         LA    R7,RFLDATA                                                       
         SR    RF,RF                                                            
         IC    RF,0(R6)            # OF LEDGERS                                 
FL18     LA    RE,8(R6)                                                         
FL20     CLC   0(2,RE),0(R7)                                                    
         BE    FL30                                                             
         LA    RE,2(RE)                                                         
         BCT   RF,FL20                                                          
         LR    R6,RE               NEXT ENTRY                                   
         B     FL12                                                             
*                                                                               
         USING FFNELD,R4                                                        
FL30     LA    R7,2(R7)                                                         
         BCT   R1,FL18                                                          
         LA    R4,ELM25                                                         
         MVI   FFNEL,FFNELQ                                                     
         MVI   FFNLN,FFNLN2Q                                                    
         MVC   FFNUMBER(7),1(R6)                                                
         B     RS35                                                             
*&&                                                                             
         SPACE 2                                                                
LDGTAB   EQU   *                                                                
         DC    AL1(0)                                                           
         EJECT                                                                  
         USING RESRECD,R3                                                       
RS70     CLI   TYPEOK,NO                                                        
         BNE   RS76                                                             
*        OC    ELM25,ELM25                                                      
*        BNZ   RS71                                                             
         OI    RESRSTA,X'80'        MARK DELETED                                
         AP    FMTDELT,=P'1'                                                    
         MVC   P+60(5),=C'TYPE='                                                
         MVC   P+65(7),=C'DELETED'                                              
         B     RS74                                                             
*                                                                               
RS71     GOTO1 ADDEL,DMCB,IO,ELM25                                              
         MVC   P+60(5),=C'TYPE='                                                
         MVC   P+65(7),ELM25+2                                                  
                                                                                
*                                                                               
RS74     AP    NOTYTOT,=P'1'                                                    
         AP    NOTYCPY,=P'1'                                                    
         MVI   WRTSW,YES                                                        
         MVC   P+1(14),=C'TYPE MISSING= '                                       
         MVC   P+16(8),IO+3                                                     
         MVC   P+30(4),=C'CHG='                                                 
         MVC   P+34(9),CHGDATE                                                  
         MVC   P+45(4),=C'REQ='                                                 
         MVC   P+49(9),REQDATE                                                  
         GOTO1 ACREPORT                                                         
*                                                                               
RS76     MVC   XKEY,IO                                                          
         CLI   WRTSW,YES                                                        
         BNE   RS80                                                             
         AP    CMPCHGS,=P'1'                                                    
         AP    TOTCHGS,=P'1'                                                    
         BAS   RE,DMPPUT           RECORD AFTER FIX                             
         CLI   RCWRITE,NO                                                       
         BE    RS80                                                             
         GOTO1 DATAMGR,DMCB,DMWRT,=C'ACCOUNT',XKEY,IO                           
         LA    R3,XKEY                                                          
         TM    RESRSTA,X'80'        MARK DELETED                                
         BO    RS82                                                             
*                                                                               
RS80     GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',XKEY,IO                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RESRECD,R3                                                       
RS82     GOTO1 DATAMGR,DMCB,DMRSEQ,=C'ACCOUNT',XKEY,IO                          
         LA    R3,IO                                                            
         CLI   RESKSEQ,RESKSREG                                                 
         BE    RS86                                                             
         CLI   RESKSEQ,RESKSTXT                                                 
         BNE   RS82                                                             
         AP    TXTRECS,=P'1'                                                    
         B     RS82                                                             
*                                                                               
RS86     AP    CMPRECS,=P'1'                                                    
         AP    TOTRECS,=P'1'                                                    
         B     RS03                                                             
         EJECT                                                                  
*              REST MONACC KEY                                                  
*                                                                               
RS90     EQU   *                                                                
*                                                                               
         BAS   RE,PRNTCPY                                                       
         MVC   P(6),=C'TOTALS'                                                  
         OI    TOTCHGS+7,X'0F'                                                  
         MVC   P+10(16),=C'RECORDS CHANGED='                                    
         UNPK  P+26(8),TOTCHGS                                                  
         OI    TOTRECS+7,X'0F'                                                  
         MVC   P+37(14),=C'TOTAL RECORDS='                                      
         UNPK  P+51(8),TOTRECS                                                  
         OI    NOTYTOT+7,X'0F'                                                  
         MVC   P+64(13),=C'MISSING TYPE='                                       
         UNPK  P+77(8),NOTYTOT                                                  
         OI    FMTDELT+7,X'0F'                                                  
         MVC   P+88(8),=C'DELETED='                                             
         UNPK  P+96(8),FMTDELT                                                  
         GOTO1 ACREPORT                                                         
*        GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',MNFLKEY,IO                       
*        CLI   DMCB+8,0                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
PRNTCPY  NTR1                                                                   
         MVC   P(5),=C'COMP='                                                   
         MVC   CMPCDE,PRVKEY+2                                                  
         GOTO1 HEXOUT,DMCB,CMPCDE,P+5,1                                         
         OI    CMPCHGS+7,X'0F'                                                  
         MVC   P+10(16),=C'RECORDS CHANGED='                                    
         UNPK  P+26(8),CMPCHGS                                                  
         OI    CMPRECS+7,X'0F'                                                  
         MVC   P+37(14),=C'TOTAL RECORDS='                                      
         UNPK  P+51(8),CMPRECS                                                  
         OI    NOTYCPY+7,X'0F'                                                  
         MVC   P+64(13),=C'MISSING TYPE='                                       
         UNPK  P+77(8),NOTYCPY                                                  
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   P+10(13),=C'TEXT RECORDS='                                       
         OI    TXTRECS+7,X'0F'                                                  
         UNPK  P+26(8),TXTRECS                                                  
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   P+10(4),=C'RCV='                                                 
         OI    RCVFMTS+7,X'0F'                                                  
         UNPK  P+14(8),RCVFMTS                                                  
*                                                                               
         MVC   P+23(4),=C'INC='                                                 
         OI    INCFMTS+7,X'0F'                                                  
         UNPK  P+27(8),INCFMTS                                                  
*                                                                               
         MVC   P+36(4),=C'PAY='                                                 
         OI    PAYFMTS+7,X'0F'                                                  
         UNPK  P+40(8),PAYFMTS                                                  
*                                                                               
         MVC   P+49(4),=C'EXP='                                                 
         OI    EXPFMTS+7,X'0F'                                                  
         UNPK  P+53(8),EXPFMTS                                                  
*                                                                               
         MVC   P+62(5),=C'PROD='                                                
         OI    PRDFMTS+7,X'0F'                                                  
         UNPK  P+67(8),PRDFMTS                                                  
*                                                                               
         MVC   P+76(5),=C'PERS='                                                
         OI    CSTFMTS+7,X'0F'                                                  
         UNPK  P+81(8),CSTFMTS                                                  
*                                                                               
         MVC   P+90(5),=C'CASH='                                                
         OI    CSHFMTS+7,X'0F'                                                  
         UNPK  P+95(8),CSHFMTS                                                  
*                                                                               
         MVC   P+104(5),=C'GL  ='                                               
         OI    GLDFMTS+7,X'0F'                                                  
         UNPK  P+109(8),GLDFMTS                                                 
*                                                                               
         MVC   P+118(5),=C'UNKN='                                               
         OI    UNKFMTS+7,X'0F'                                                  
         UNPK  P+123(8),UNKFMTS                                                 
         GOTO1 ACREPORT                                                         
         ZAP   CMPCHGS,=P'0'                                                    
         ZAP   CMPRECS,=P'0'                                                    
         ZAP   RCVFMTS,=P'0'                                                    
         ZAP   INCFMTS,=P'0'                                                    
         ZAP   PAYFMTS,=P'0'                                                    
         ZAP   EXPFMTS,=P'0'                                                    
         ZAP   PRDFMTS,=P'0'                                                    
         ZAP   CSTFMTS,=P'0'                                                    
         ZAP   CSHFMTS,=P'0'                                                    
         ZAP   GLDFMTS,=P'0'                                                    
         ZAP   UNKFMTS,=P'0'                                                    
         ZAP   TXTRECS,=P'0'                                                    
         ZAP   NOTYCPY,=P'0'                                                    
         B     XIT                                                              
         EJECT                                                                  
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
DMPGET   CLI   QOPT1,YES           MUST REQUEST DUMP                            
         BNER  RE                                                               
         CLI   DMPSW,YES           ALREADY DUMPED THIS RECORD                   
         BNER  RE                                                               
         MVI   DMPSW,NO                                                         
         NTR1  ,                                                                
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,TEXTMSG                                                       
         ICM   R6,8,=AL1(L'TEXTMSG)                                             
         CLC   TEXTMSG,SPACES                                                   
         BH    DUMP                                                             
         LA    R6,=C'GET'                                                       
         ICM   R6,8,=AL1(3)                                                     
         B     DUMP                                                             
*                                                                               
DMPPUT   CLI   QOPT1,YES           MUST REQUEST DUMP                            
         BNER  RE                                                               
         CLI   WRTSW,YES           DID WE WRITE PUT THIS RECORD                 
         BNER  RE                  NO, SO DON'T SHOW PUT                        
         NTR1  ,                                                                
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         LA    R6,TEXTMSG                                                       
         ICM   R6,8,=AL1(L'TEXTMSG)                                             
         CLC   TEXTMSG,SPACES                                                   
         BH    DUMP                                                             
         LA    R6,=C'PUT'                                                       
         ICM   R6,8,=AL1(3)                                                     
         SPACE 1                                                                
         USING RESRECD,R3                                                       
DUMP     SR    R8,R8                                                            
         ICM   R8,3,RESRLEN                                                     
         GOTO1 PRNTBL,DMCB,(R6),(R3),C'DUMP',(R8),=C'2D'                        
         XC    TEXTMSG,TEXTMSG                                                  
         B     XIT                                                              
*                                                                               
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
WRTSW    DS    CL1                                                              
DMPSW    DS    CL1                                                              
         EJECT                                                                  
HEXIN    DC    V(HEXIN)                                                         
         LTORG                                                                  
         EJECT                                                                  
ACXVD    DSECT                                                                  
FILTSEL  DS    XL1                                                              
NEWTYPE  DS    CL1                                                              
NEWCODE  DS    CL6                                                              
SAVELDG  DS    CL2                                                              
CMPCDE   DS    XL1                                                              
CMPRECS  DS    PL8                                                              
CMPCHGS  DS    PL8                                                              
TOTRECS  DS    PL8                                                              
TOTCHGS  DS    PL8                                                              
RCVFMTS  DS    PL8                                                              
INCFMTS  DS    PL8                                                              
PAYFMTS  DS    PL8                                                              
PRDFMTS  DS    PL8                                                              
EXPFMTS  DS    PL8                                                              
CSTFMTS  DS    PL8                                                              
CSHFMTS  DS    PL8                                                              
GLDFMTS  DS    PL8                                                              
UNKFMTS  DS    PL8                                                              
TXTRECS  DS    PL8                                                              
NOTYTOT  DS    PL8                                                              
NOTYCPY  DS    PL8                                                              
FMTDELT  DS    PL8                                                              
REQDATE  DS    CL9                                                              
CHGDATE  DS    CL9                                                              
TYPEOK   DS    CL1                                                              
ELM      DS    CL255                                                            
ELM25    DS    CL255                                                            
MNFLKEY  DS    CL42                                                             
XKEY     DS    CL42                                                             
PRVKEY   DS    CL42                                                             
TEXTMSG  DS    CL12                                                             
IO       DS    CL2100                                                           
         EJECT                                                                  
*  ACREPWORKD                                                                   
*  ACGENBOTH                                                                    
*  ACGENMODES                                                                   
*  DMDTFIS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE DMDTFIS                                                        
*ACDDEQUS                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'070ACREPXV02 05/01/02'                                      
         END                                                                    
