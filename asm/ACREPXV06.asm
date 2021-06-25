*          DATA SET ACREPXV06  AT LEVEL 102 AS OF 12/17/99                      
*PHASE ACXV02C                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE DICTATE                                                                
*INCLUDE SCANNER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'PRINT FOR ANALYSIS DELETED RECORDS'                             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*        QOPT1 = Y THEN DUMP OUT RECORDS                              *         
*        QOPT2 = Y THEN SKIP DELETED DRAFT TRANSACTIONS               *         
*        QOPT3 = Y THEN READ FOR DELETED SPECIAL RECORDS TOO          *         
*        QOPT3 = O THEN READ FOR DELETED SPECIAL RECORDS ONLY         *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
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
         MVI   DMPSW,0                                                          
         CLI   QOPT1,YES                                                        
         BNE   *+8                                                              
         OI    DMPSW,DMPYES                                                     
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
         USING ACTRECD,R3                                                       
DT20     LA    R3,XKEY                                                          
         MVC   XKEY,SPACES                                                      
         MVC   XDIR,SPACES                                                      
         CLI   QOPT3,YES           READ SPECIAL RECORDS TOO ?                   
         BNE   DT25                NO                                           
         XC    XKEY,XKEY                                                        
         XC    XDIR,XDIR                                                        
*                                                                               
DT25     CLI   FILTSEL,0                                                        
         BE    *+10                                                             
         MVC   ACTKCPY,FILTSEL                                                  
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=CL8'ACCDIR',XKEY,XDIR               
         TM    DMCB+8,0                                                         
         BZ    *+6                                                              
         DC    H'00'                                                            
*                                                                               
RS03     LA    R3,XDIR                                                          
         CLI   ACTKCPY,X'FF'                                                    
         BE    RS90                                                             
         CLI   FILTSEL,0                                                        
         BE    RS10                                                             
         CLC   FILTSEL,ACTKCPY                                                  
         BL    RS90                                                             
         CLC   QUNIT(2),SPACES                                                  
         BE    RS10                                                             
         CLC   ACTKEY+1(2),QUNIT                                                
         BNE   RS80                                                             
         CLC   ACTKEY+3(12),SPACES                                              
         BE    RS10                                                             
         CLC   ACTKEY+3(12),QACCOUNT                                            
         BNE   RS80                                                             
*                                                                               
RS10     TM    ACTKSTAT,ACTSDELT                                                
         BZ    RS80                                                             
         CLI   QOPT3,ONLY                                                       
         BNE   RS12                                                             
         CLC   0(R3),X'40'                                                      
         BNL   RS90                                                             
*                                                                               
RS12     MVI   RECSW,0             IS IT A DRAFT TRANSACTION ?                  
         MVC   DA,ACTKDA                                                        
         LA    R3,IO                                                            
         GOTO1 DATAMGR,DMCB,(X'08',DMGET),=CL8'ACCMST',DA,(R3),XWRK             
         CLI   TRNRFST-TRNRECD(R3),TRNELQ  X'44'                                
         BNE   RS15                                                             
         OI    RECSW,RECTRN                                                     
         LA    R3,XDIR                                                          
         TM    ACTKSTAT,TRNSDRFT                                                
         BZ    RS15                                                             
         OI    RECSW,RECDFT                                                     
*                                                                               
RS15     LA    R3,IO                                                            
         MVI   WRTSW,NO            SET WRITE SWITCH                             
         BAS   RE,DUMP                                                          
*                                                                               
RS80     GOTO1 DATAMGR,DMCB,(X'08',DMRSEQ),=CL8'ACCDIR',XKEY,XDIR               
         AP    CMPRECS,=P'1'                                                    
         AP    TOTRECS,=P'1'                                                    
         B     RS03                                                             
         EJECT                                                                  
RS90     EQU   *                                                                
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
         LA    R3,IO                                                            
         SR    R8,R8                                                            
         ICM   R8,3,ACTRLEN                                                     
         LA    R4,=CL30'MARKED DELETED'                                         
         TM    RECSW,RECTRN                                                     
         BZ    DUMP20                                                           
         LA    R4,=CL30'TRANSACTION MARKED DELETED'                             
         TM    RECSW,RECDFT                                                     
         BZ    DUMP20                                                           
         LA    R4,=CL30'MARKED DRAFTED && DELETED'                              
         CLI   QOPT2,YES           SKIP DRAFT TRANSACTIONS ?                    
         BE    XIT                                                              
*                                                                               
DUMP20   TM    DMPSW,DMPYES                                                     
         BZ    DUMP30                                                           
         GOTO1 PRNTBL,DMCB,(30,(R4)),(R3),C'DUMP',(R8),=C'2D'                   
         B     XIT                                                              
*                                                                               
DUMP30   CLI   0(R3),X'40'                                                      
         BH    DUMP35                                                           
         LA    R4,=CL30'SPECIAL RECORD'                                         
         GOTO1 PRNTBL,DMCB,(30,(R4)),(R3),C'DUMP',(R8),=C'2D'                   
         B     XIT                                                              
*                                                                               
         USING TRNRECD,R3                                                       
DUMP35   GOTO1 HEXOUT,DMCB,TRNKCPY,P+5,1                                        
         MVC   P+10(14),TRNKULA                                                 
         MVC   P+27(2),TRNKOFF                                                  
         CLC   TRNKULC,SPACES                                                   
         BNH   DUMP38                                                           
         CLC   TRNKUNT(2),=C'SR'                                                
         BE    DUMP37                                                           
         GOTO1 HEXOUT,DMCB,TRNKCCPY,P+32,1                                      
*                                                                               
DUMP37   MVC   P+35(14),TRNKULC                                                 
*                                                                               
DUMP38   TM    RECSW,RECTRN                                                     
         BO    DUMP39                                                           
         CLC   TRNKREF,=C'*TIME*'                                               
         BE    DUMP39                                                           
         CLC   TRNKDATE,SPACES                                                  
         BNH   DUMP40                                                           
         MVC   P+52(3),TRNKDATE                                                 
         B     DUMP40                                                           
*                                                                               
DUMP39   GOTO1 DATCON,DMCB,(1,TRNKDATE),(8,P+52)                                
*                                                                               
DUMP40   CLC   TRNKREF,SPACES                                                   
         BNH   DUMP80                                                           
         MVC   P+62(6),TRNKREF                                                  
*                                                                               
DUMP80   TM    RECSW,RECDFT                                                     
         BZ    DUMP88                                                           
         MVC   P+70(10),=CL10'DRAFT'                                            
*                                                                               
DUMP88   GOTO1 ACREPORT                                                         
*                                                                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
TURNOFF  EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
ONLY     EQU   C'O'                                                             
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
         EJECT                                                                  
HEXIN    DC    V(HEXIN)                                                         
         LTORG                                                                  
         EJECT                                                                  
ACX4D    DSECT                                                                  
WRTSW    DS    CL1                                                              
DMPSW    DS    CL1                                                              
DMPYES   EQU   X'80'                                                            
RECSW    DS    XL1                                                              
RECTRN   EQU   X'80'               TRANSACTION                                  
RECDFT   EQU   X'40'               DRAFT TRANSACTION                            
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
XKEY     DS    CL32                                                             
XDIR     DS    CL60                                                             
ACKEY    DS    CL64                                                             
DA       DS    XL4                                                              
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
IO       DS    CL2050                                                           
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
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'102ACREPXV06 12/17/99'                                      
         END                                                                    
