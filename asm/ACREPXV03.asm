*          DATA SET ACREPXV03  AT LEVEL 051 AS OF 05/01/02                      
*PHASE ACXV02A                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE DICTATE                                                                
*INCLUDE SCANNER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'SCRIBE CONVERT ELEMENT DATE INFO'                               
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
         MVI   COSTFG,NO                                                        
         ZAP   TOTRECS,=P'0'                                                    
         ZAP   TOTCHGS,=P'0'                                                    
         ZAP   CMPRECS,=P'0'                                                    
         MVI   FORCEHED,YES                                                     
         MVI   FCRESET,YES                                                      
         MVI   FILTSEL,0                                                        
         GOTO1 HEXIN,DMCB,QSELECT,FILTSEL,2                                     
         OC    DMCB+12(4),DMCB+12                                               
         BNZ   *+8                                                              
         MVI   FILTSEL,0                                                        
         XC    TEXTMSG,TEXTMSG                                                  
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
DT20     LA    R3,XKEY                                                          
         USING ACKEYD,R3                                                        
         MVI   ACCSTYPE,ACCSEQU    SET KEY FOR RS RECORDS                       
         MVI   ACCSSREC,ACCSSEQU                                                
         CLI   FILTSEL,0                                                        
         BZ    *+10                                                             
         MVC   ACCSCMP,FILTSEL                                                  
RS02     GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',XKEY,IO                          
         LA    R3,IO                                                            
*                                                                               
RS03     CLC   IO(2),XKEY          CHECK RECORD TYPE                            
         BNE   RS90                                                             
         CLC   IO(3),PRVKEY        SAME COMP CODE?                              
         BE    RS20                                                             
         MVI   COSTFG,NO                                                        
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
         ZAP   CMPCHGS,=P'0'                                                    
         ZAP   CMPRECS,=P'0'                                                    
         CLI   FILTSEL,0           COMPANY FILTERING                            
         BZ    RS20                YES, SHOW ONLY ONE COMPANY                   
         CLC   FILTSEL,ACCSCMP                                                  
         BNE   RS95                                                             
*                                                                               
RS20     MVC   PRVKEY,IO                                                        
         MVI   WRTSW,NO            SET WRITE SWITCH                             
         MVI   DMPSW,YES           SET DUMP SWITCH                              
         MVI   ALLRDY,NO                                                        
*                                                                               
RS24     LA    R3,IO                                                            
         LA    R4,ACRECORD                                                      
         CLI   FILTSEL,0                                                        
         BE    RS30                                                             
         CLC   FILTSEL,2(R3)       MATCH ON COMPANY                             
         BNE   RS70                                                             
*                                                                               
RS30     CLI   0(R4),0                                                          
         BE    RS70                                                             
         CLI   0(R4),FFNELQ        X'25' REPORT TYPE (FREE FORM)                
         BE    FF10                                                             
         CLI   0(R4),RRWELQ        X'C2' ROW                                    
         BE    RW10                                                             
         CLI   0(R4),RCLELQ        X'C3' COLUMN                                 
         BE    CL10                                                             
         CLI   0(R4),RPFELQ        X'C4' PROFILE ELEMENT                        
         BE    PF10                                                             
RS35     SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     RS30                                                             
         EJECT                                                                  
         USING FFNELD,R4                                                        
FF10     MVC   P,SPACES                                                         
         CLC   3(6,R4),=CL6'COST'                                               
         BNE   FF12                                                             
         CLI   2(R4),C'0'          COST?                                        
         BE    FF12                                                             
         MVC   P+12(1),2(R4)                                                    
         MVC   P+2(8),3(R3)                                                     
         GOTO1 ACREPORT                                                         
         B     RS35                                                             
*                                                                               
FF12     CLI   2(R4),C'0'          COST?                                        
         BNE   RS35                                                             
         CLC   3(6,R4),=CL6'PERSON'                                             
         BE    RS35                                                             
         CLI   COSTFG,YES                                                       
         BE    FF20                                                             
         MVI   COSTFG,YES                                                       
         MVC   P(14),=C'COST TO PERSON'                                         
         GOTO1 ACREPORT                                                         
*                                                                               
FF20     MVC   3(6,R4),=CL6'PERSON'                                             
         MVI   WRTSW,YES              WRITE IT BACK                             
         B     RS35                                                             
         DROP  R4                                                               
         EJECT                                                                  
         USING RCLELD,R4                                                        
CL10     DS    0H                  COLUMN ELEMENT                               
         TM    RCLOPT,RCLMDTE      MOA DATE                                     
         BZ    RS35                                                             
         MVI   RCLDATES,RCLMODT                                                 
         BO    CL30                                                             
         MVI   RCLDATES,RCLTRDT                                                 
         TM    RCLOPT,RCLBDTE      TRANSACTION DATE                             
         BO    CL30                                                             
         MVI   RCLDATES,RCLDUDT                                                 
         TM    RCLOPT,RCLDDTE      DUE DATE                                     
         BO    *+6                                                              
         DC    H'00'                                                            
CL30     NI    RCLOPT,X'FF'-RCLMDTE                                             
         MVI   WRTSW,YES              WRITE IT BACK                             
         B     RS35                                                             
         DROP  R4                                                               
         EJECT                                                                  
         USING RRWELD,R4                                                        
RW10     DS    0H                  COLUMN ELEMENT                               
         TM    RRWOPT,RRWMDTE      MOA DATE                                     
         BZ    RS35                                                             
         MVI   RRWDATES,RRWMODT                                                 
         BO    RW30                                                             
         MVI   RRWDATES,RRWTRDT                                                 
         TM    RRWOPT,RRWBDTE      TRANSACTION DATE                             
         BO    RW30                                                             
         MVI   RRWDATES,RRWDUDT                                                 
         TM    RRWOPT,RRWDDTE      DUE DATE                                     
         BO    *+6                                                              
         DC    H'00'                                                            
RW30     NI    RRWOPT,X'FF'-RRWMDTE                                             
         MVI   WRTSW,YES              WRITE IT BACK                             
         B     RS35                                                             
         DROP  R4                                                               
         EJECT                                                                  
         USING RPFELD,R4                                                        
PF10     DS    0H                     PROFILE ELEMENT                           
         MVC   P,SPACES                                                         
         CLC   RPFMETHD,SPACES                                                  
         BNH   PF12                                                             
         MVC   P+2(8),3(R3)                                                     
         MVC   P+14(12),=C'METHOD FOUND'                                        
         MVC   P+27(1),RPFMETHD                                                 
*                                                                               
PF12     CLC   RPFLIST,SPACES                                                   
         BNH   PF20                                                             
         MVC   P+2(8),3(R3)                                                     
         MVC   P+40(14),=C'OLD LIST FOUND'                                      
         MVC   P+58(6),RPFLIST                                                  
*                                                                               
PF20     CLC   P(10),SPACES                                                     
         BE    RS35                                                             
         GOTO1 ACREPORT                                                         
         B     RS35                                                             
         EJECT                                                                  
         USING RFLELD,R4                                                        
FL10     DS    0H                     FILTER ELEMENT                            
         B     RS35                                                             
         EJECT                                                                  
RS70     MVC   XKEY,IO                                                          
         CLI   WRTSW,YES                                                        
         BNE   RS80                                                             
         AP    CMPCHGS,=P'1'                                                    
         AP    TOTCHGS,=P'1'                                                    
         BAS   RE,DMPPUT           RECORD AFTER FIX                             
         CLI   RCWRITE,NO                                                       
         BE    RS80                                                             
         GOTO1 DATAMGR,DMCB,DMWRT,=C'ACCOUNT',XKEY,IO                           
*                                                                               
RS80     GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',XKEY,IO                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,DMRSEQ,=C'ACCOUNT',XKEY,IO                          
         AP    CMPRECS,=P'1'                                                    
         AP    TOTRECS,=P'1'                                                    
         MVI   WRTSW,NO            SET WRITE SWITCH                             
         B     RS03                                                             
         EJECT                                                                  
*              REST MONACC KEY                                                  
*                                                                               
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
         DROP  R4                                                               
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
DUMP     SR    R8,R8                                                            
         ICM   R8,3,ACLENGTH                                                    
         GOTO1 PRNTBL,DMCB,(R6),(R3),C'DUMP',(R8),=C'2D'                        
         XC    TEXTMSG,TEXTMSG                                                  
         B     XIT                                                              
*                                                                               
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
         EJECT                                                                  
HEXIN    DC    V(HEXIN)                                                         
*                                                                               
TTYPETAB DS    0C                                                               
         DC    AL1(TY30DI)        DIFFERENCE TYPE 30                            
         DC    CL4'DI'                                                          
         DC    AL1(TY30CH)        CHECK TYPE 30                                 
         DCDD  AC#RSTCH,4                                                       
         DC    AL1(TY30OF)        OFFSET TYPE 30                                
         DCDD  AC#RSTOF,4                                                       
         DC    AL1(TY30WO)        WRITE-OFF TYPE 30                             
         DCDD  AC#RSTWO,4                                                       
         DC    AL1(TY30TT)        TRANSFERED FROM TYPE 30                       
         DCDD  AC#RSTTT,4                                                       
         DC    AL1(TY30TF)        TRANSFERED TO TYPE 30                         
         DCDD  AC#RSTTF,4                                                       
         DC    AL1(TY06MN)        MANUAL BILLING TYPE 06                        
         DC    CL4'M'                                                           
         DC    AL1(EOT)                                                         
         LTORG                                                                  
         EJECT                                                                  
ACX4D    DSECT                                                                  
RQSTART  DS    PL3                                                              
RQEND    DS    PL3                                                              
ALLRDY   DS    XL1                                                              
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
XKEY     DS    CL42                                                             
PRVKEY   DS    CL42                                                             
TEXTMSG  DS    CL12                                                             
NPARMS   DS    XL1                 # OF PARAMETERS                              
BLOCK    DS    6CL32               DATA BLOCK FOR SCANNER                       
DUMFLDH  DS    CL8                 DUMMY FIELD HEADER                           
DUMFLD   DS    CL64                                                             
TYPECDE  DS    CL4                                                              
IO       DS    CL2100                                                           
COSTFG   DS    CL1                                                              
         EJECT                                                                  
ESCHIGHQ EQU   48                                                               
*        SUBDIVISION OF BATCH TYPES                                             
TY30DI   EQU   229                 TYPE 30 DIFFERENCE (FOREIGN CURR)            
TY06MN   EQU   230                 TYPE 06 MANUAL BILLING                       
TY30CH   EQU   231                 TYPE 30 CHECK                                
TY30OF   EQU   232                 TYPE 30 OFFSET                               
TY30WO   EQU   233                 TYPE 30 WRITE OFF                            
TY30TT   EQU   234                 TYPE 30 TRANSFER TO                          
TY30TF   EQU   235                 TYPE 30 TRANSFER FROM                        
         EJECT                                                                  
*  ACREPWORKD                                                                   
*  ACGENBOTH                                                                    
*  ACGENMODES                                                                   
*  DMDTFIS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE DMDTFIS                                                        
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051ACREPXV03 05/01/02'                                      
         END                                                                    
