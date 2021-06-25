*          DATA SET ACREPXV0A  AT LEVEL 136 AS OF 03/13/98                      
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
         L     R5,ADMASTC                                                       
         USING MASTD,R5                                                         
         L     R2,MCUTL                                                         
         MVC   SAVSE,4(R2)                                                      
         MVI   4(R2),X'0A'                                                      
         GOTO1 DATAMGR,DMCB,(X'80',DMRDHI),=CL8'GENDIR',XKEY,XDIR               
         TM    DMCB+8,0                                                         
         BZ    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         LA    R3,XDIR                                                          
*S03     CLC   GRPKAGY,=C'JW'                                                   
*        BNE   RS80                                                             
RS03     CLC   =X'002F',XDIR       CHECK RECORD TYPE                            
         BNE   RS90                FINISHED                                     
         CLI   GRPKSYST,C'A'       ACCOUNTING                                   
         BNE   RS80                                                             
*        CLC   GRPKUSER,=AL2(0408)                                              
*        BNE   RS80                                                             
         MVC   DA,XDIR+36                                                       
         LA    R3,IO                                                            
         GOTO1 DATAMGR,DMCB,(X'80',DMGET),=CL8'GENFILE',DA,(R3),XWRK            
         MVI   WRTSW,NO            SET WRITE SWITCH                             
         MVI   DMPSW,NO                                                         
         LA    R8,L'GRPKEY                                                      
         GOTO1 PRNTBL,DMCB,=C'KEY',(R3),C'DUMP',(R8),=C'2D'                     
*                                                                               
RS24     LA    R4,GRPFSTEL                                                      
RS30     CLI   0(R4),0                                                          
         BE    RS70                                                             
         CLI   0(R4),GRPHCDQ       X'01' HEADER ELEMENT                         
         BE    ELEM01                                                           
         CLI   0(R4),GRPRCDQ       X'30' REQ CARD ELEMENT                       
         BE    ELEM30                                                           
RS35     SR    R1,R1                                                            
         IC    R1,1(,R4)                                                        
         AR    R4,R1                                                            
         B     RS30                                                             
         EJECT                                                                  
         USING GRPHD,R4                                                         
*LEM01   CLC   GRPHDEST,=AL2(0408)                                              
*        BNE   RS35                                                             
ELEM01   SR    R8,R8                                                            
         IC    R8,1(R4)                                                         
         GOTO1 PRNTBL,DMCB,=C'HEADER',(R4),C'DUMP',(R8),=C'2D'                  
         B     RS35                                                             
         DROP  R4                                                               
         EJECT                                                                  
         USING GRPRD,R4                                                         
ELEM30   CLC   GRPRRNUM,=C'VL'                                                  
         BNE   RS35                                                             
*                                                                               
         CLI   GRPRCRDN,1                                                       
         BNE   ELEM30A                                                          
         LA    R2,=C'CARD1'                                                     
         LA    R6,CARD1            POINT AT CARDS 1 AND 2                       
         MVC   CARD1,GRPCARD                                                    
         B     ELEM30C                                                          
*                                                                               
ELEM30A  CLI   GRPRCRDN,2                                                       
         BNE   ELEM30B             NEXT ELEMENT                                 
         LA    R2,=C'CARD2'                                                     
         LA    R6,CARD2                                                         
         MVC   CARD2,GRPCARD                                                    
         B     ELEM30C                                                          
*                                                                               
ELEM30B  CLI   GRPRCRDN,3                                                       
         BNE   RS35                NEXT ELEMENT                                 
         LA    R2,=C'CARD3'                                                     
         LA    R6,CARD3                                                         
         MVC   CARD3,GRPCARD                                                    
*                                                                               
         USING ACQD,R6                                                          
ELEM30C  CLI   QOPT2,C'Y'                                                       
         BNE   RS35                                                             
         LA    R8,80               ELEMENT LENGTH                               
         GOTO1 PRNTBL,DMCB,(5,(R2)),(R6),C'DUMP',(R8),=C'2D'                    
         B     RS35                MUST BE OK                                   
*                                                                               
*&&DO                                                                           
RS52     SR    R8,R8                                                            
         IC    R8,1(R4)            ELEMENT LENGTH                               
         GOTO1 PRNTBL,DMCB,=C'HEADER',GRPKAGY,C'DUMP',25,=C'2D'                 
         GOTO1 PRNTBL,DMCB,=C'REQUEST',(R4),C'DUMP',(R8),=C'2D'                 
         B     RS35                NEXT ELEMENT                                 
         DROP  R4,R6                                                            
*&&                                                                             
         EJECT                                                                  
RS70     CLI   WRTSW,YES                                                        
         BNE   RS80                                                             
         AP    CMPCHGS,=P'1'                                                    
         CLI   RCWRITE,YES                                                      
         BNE   RS80                                                             
         LA    R3,IO                                                            
         GOTO1 DATAMGR,DMCB,DMPUT,=CL8'GENFILE',DA,(R3),XWRK                    
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
CARD1    DS    CL80                                                             
CARD2    DS    CL80                                                             
CARD3    DS    CL80                                                             
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
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'136ACREPXV0A 03/13/98'                                      
         END                                                                    
