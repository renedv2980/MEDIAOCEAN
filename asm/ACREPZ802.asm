*          DATA SET ACREPZ802  AT LEVEL 035 AS OF 08/16/00                      
*PHASE ACZ802A                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'DELETE 1R 1C OLD COST BUCKET ELS'                               
         PRINT NOGEN                                                            
ACZ802   CSECT                                                                  
         NMOD1 0,**ACZ8**,RR=R7                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACZ8D,RC                                                         
         EJECT                                                                  
         CLI   MODE,RUNFRST                                                     
         BNE   CA2                                                              
         ZAP   CHARCD,=P'0'                                                     
         ZAP   DRS,=P'0'                                                        
         ZAP   CRS,=P'0'                                                        
         ST    R7,RELO                                                          
         L     R1,HELLO                                                         
         A     R1,RELO                                                          
         ST    R1,HELLO                                                         
         L     R1,PRNTBL                                                        
         A     R1,RELO                                                          
         ST    R1,PRNTBL                                                        
         OPEN  (TAPEOUT,(OUTPUT))                                               
         SPACE 2                                                                
XIT      XMOD1 1                                                                
         EJECT                                                                  
CA2      CLI   MODE,REQFRST                                                     
         BNE   CA10                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         ZAP   DUMPCNT,=P'0'                                                    
         ZAP   DUMPCNT2,=P'0'                                                   
         ZAP   PDUMP,=P'0'                                                      
         MVC   FRSTEL,=Y(ACCRFST-ACCRECD)                                       
         B     XIT                                                              
         EJECT                                                                  
CA10     CLI   MODE,PROCACC                                                     
         BNE   CA70                                                             
         MVC   DIO(L'ACCKEY),SPACES                                             
         L     RF,ADACC                                                         
         MVC   DIO(L'ACCKEY),0(RF)                                              
         CLC   DIO+1(2),=C'1C'                                                  
         BNE   CA11                                                             
         MVC   DIO+17(1),RCCOMPFL    IF 1C START WITH CONTRA 14                 
         MVC   DIO+18(2),=C'14'                                                 
*                                                                               
         USING CACRECD,R3                                                       
CA11     LA    R3,DIO                                                           
         OI    DMINBTS,X'08'                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),DIR,DIO,DIO                        
         TM    8(R1),X'FD'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CA11A    DS    0H                                                               
         LA    R3,DIO                                                           
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRSEQ),DIR,DIO,DIO                        
         TM    8(R1),X'FD'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     RF,ADACC                                                         
         CLC   0(15,RF),DIO                                                     
         BNE   XIT                                                              
*                                  IDENTIFY A HISTORY RECORD                    
         CLC   CACKBTYP,SPACES     NO BUCKET TYPE                               
         BNE   CA11A                                                            
         CLC   CACKCACT,SPACES     MUST HAVE CONTRA                             
         BE    CA11A                                                            
         CLC   CACKSPAC,SPACES     AND NO TRANSACTION DATE                      
         BNE   CA11A                                                            
         CLC   CACKSTYP,SPACES                                                  
         BNE   CA11A                                                            
         CLC   CACKUNT(2),=C'1C'                                                
         BNE   CA12                                                             
         CLC   CACKCUNT(2),=C'14'                                               
         BE    CA13                                                             
         CLC   CACKCUNT(2),=C'15'                                               
         BE    CA13                                                             
         B     CA11A                                                            
CA12     DS    0H                                                               
         CLC   CACKUNT(2),=C'1R'                                                
         BNE   XIT                                                              
CA13     MVC   DA,CACKDA                                                        
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),ACCMST,DA,IO,DMWRK                 
         TM    8(R1),X'FD'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,IO                                                            
         MVI   ACSW,C'N'                                                        
         LR    R2,R3                                                            
         AH    R2,FRSTEL                                                        
CA14     CLI   0(R2),0                                                          
         BE    CA20                                                             
         CLI   0(R2),X'45'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUKELD,R2                                                        
         CLC   BUKYEAR,=X'97'                                                   
         BNE   CA18                                                             
         AP    DRS,BUKDR                                                        
         AP    CRS,BUKCR                                                        
         CLI   ACSW,C'Y'           SET RECORD CHANGED                           
         BE    *+8                                                              
         BAS   RE,DMPGET                                                        
         MVI   BUKEL,DELELQ        MARK IT DELETED                              
         MVI   ACSW,C'Y'           SET RECORD CHANGED                           
CA18     SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     CA14                                                             
*                                                                               
CA20     CLI   ACSW,C'Y'           RECORD CHANGED?                              
         BNE   CA11A                                                            
         GOTO1 HELLO,ELIST,(C'D',ACCMST),('DELELQ',CACRECD),0                   
         LA    R2,IO                                                            
         AH    R2,FRSTEL                                                        
CA22     CLI   0(R2),0                                                          
         BE    CA28                                                             
         CLI   0(R2),X'45'                                                      
         BE    CA30                                                             
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     CA22                                                             
CA28     DS    0H                                                               
         GOTO1 HELLO,ELIST,(C'P',ACCMST),CACRECD,EL45SAVE,0                     
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
CA30     CLI   RCWRITE,C'N'                                                     
         BE    CA50                                                             
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,DA,IO,DMWRK                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         AP    CHARCD,=P'1'                                                     
*                                                                               
CA50     BAS   RE,DMPPUT                                                        
         B     CA11A                                                            
         EJECT                                                                  
CA70     CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         GOTO1 ACREPORT                                                         
         MVC   P+1(16),=C'RECORDS CHANGED'                                      
         EDIT  CHARCD,(16,P+30),COMMAS=YES                                      
         GOTO1 ACREPORT                                                         
         MVC   P+1(16),=C'DEBITS DELETED '                                      
         EDIT  DRS,(16,P+30),2,COMMAS=YES                                       
         GOTO1 ACREPORT                                                         
         MVC   P+1(16),=C'CREDITS DELETED'                                      
         EDIT  CRS,(16,P+30),2,COMMAS=YES                                       
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
DMPGET   NTR1                                                                   
         MVI   DMPSW,C'N'                                                       
         CLI   QOPT1,C'Y'                                                       
         BNE   XIT                                                              
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'GET'                                                       
         MVI   DMPSW,C'Y'                                                       
         B     DUMP                                                             
*                                                                               
DMPPUT   NTR1                                                                   
         CLI   DMPSW,C'Y'                                                       
         BNE   XIT                                                              
         LA    R6,=C'PUT'                                                       
*                                                                               
DUMP     SR    R5,R5                                                            
         ICM   R5,3,ACCRLEN-ACCRECD(R3)                                         
         C     R3,=A(DIO)                                                       
         BNE   *+8                                                              
         LH    R5,=Y(L'DIO)                                                     
DUMP01   DS    0H                                                               
         GOTO1 PRNTBL,DMCB,(3,(R6)),(R3),C'DUMP',(R5),=C'2D'                    
         B     XIT                                                              
         EJECT                                                                  
*        CONSTANTS                                                              
*                                                                               
DELELQ   EQU   X'FF'               X'FF' FOR ELEMENT DELETION                   
*                                                                               
*                                                                               
RELO     DC    F'0'                                                             
HELLO    DC    V(HELLO)                                                         
PRNTBL   DC    V(PRNTBL)                                                        
*                                                                               
*                                                                               
DMPSW    DC    C'N'                                                             
DUMPCNT  DC    PL4'0'                                                           
DUMPCNT2 DC    PL4'0'                                                           
DRS      DC    PL8'0'                                                           
CRS      DC    PL8'0'                                                           
DROPHR   DC    PL8'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'0000100'                                                     
*                                                                               
FRSTEL   DC    H'49'                                                            
ACCFIL   DC    CL8'ACCOUNT'                                                     
ACCOUNT  DC    CL8'ACCOUNT'                                                     
ACCMST   DC    CL8'ACCMST'                                                      
DIR      DC    CL6'ACCDIR'                                                      
GETREC   DC    CL8'GETREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
ADDREC   DC    CL8'ADDREC'                                                      
EL45SAVE DC    XL16'4510961200000000000C00000000000C'                           
DIO      DS    CL(CACRFST-CACRECD)                                              
         EJECT                                                                  
         LTORG                                                                  
                                                                                
TAPEOUT  DCB   DDNAME=TAPEOUT,RECFM=VB,DSORG=PS,MACRF=(PM),            X        
               BLKSIZE=32760,LRECL=4004,BUFNO=2                                 
         EJECT                                                                  
*              DSECT FOR PROGRAM                                                
*                                                                               
ACZ8D    DSECT                                                                  
ELCODE   DS    CL1                                                              
COMMAND  DS    CL6                                                              
START    DS    XL3                                                              
END      DS    XL3                                                              
*                                                                               
ELIST    DS    3F                  FOR GETL, ADDEL, DELEL                       
ELERR    DS    CL1                                                              
         ORG   ELERR               ERROR CODE FROM HELLO                        
ELADDR   DS    F                   ADDRESS OF ELEMENT FROM HELLO                
         DS    2F                                                               
*                                                                               
ACSW     DS    CL1                                                              
CHARCD   DS    PL6                                                              
DA       DS    F                                                                
DMWRK    DS    12D                                                              
IOREC    DS    0CL2004                                                          
IOH      DS    CL2                       LENGTH                                 
         DS    CL2                       N/D                                    
IOHL     EQU   *-IOH                                                            
IO       DS    CL2000                                                           
DA2      DS    F                                                                
DMWRK2   DS    12D                                                              
         EJECT                                                                  
*        ACGENBOTH                                                              
*        ACGENFILE                                                              
*        ACGENPOST                                                              
*        ACMASTD                                                                
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035ACREPZ802 08/16/00'                                      
         END                                                                    
**PAN#1  CSECT                                                                  
         END                                                                    
